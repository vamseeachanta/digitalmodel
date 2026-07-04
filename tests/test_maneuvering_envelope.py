"""Tests for the low-speed manoeuvring & station-keeping envelope.

Golden values are the adversarially-verified SIROCCO (B1528) numbers:
    Lbp 225.5 m, B 32.26 m, T 12.2 m (laden), Cb 0.82,
    rudder area 44.94 m², span 9.0 m, Barrass lever x_R = 0.6*Lbp = 135.3 m.
References reproduced: Clarke (1983), Whicker & Fehlner (1958),
Nomoto (1957), Soeding (1982), OCIMF (1994), IMO MSC.137(76).
"""
import math

import pytest

from digitalmodel.hydrodynamics.propeller_rudder import RudderGeometry, VesselPropulsion
from digitalmodel.naval_architecture import maneuvering_envelope as me

L, B, T, CB = 225.5, 32.26, 12.2, 0.82
A_R, SPAN = 44.94, 9.0
X_R = 0.6 * L


# --- Clarke derivatives + course stability ---------------------------------

def test_clarke_common_factor():
    d = me.clarke_derivatives(L, B, T, CB)
    assert d["k"] == pytest.approx(0.009196, abs=1e-5)


def test_clarke_velocity_derivatives():
    d = me.clarke_derivatives(L, B, T, CB)
    assert d["Yv"] == pytest.approx(-0.01717, abs=2e-4)
    assert d["Nr"] == pytest.approx(-0.00251, abs=2e-4)
    assert d["m"] == pytest.approx(0.01269, abs=2e-4)


def test_full_tanker_marginally_unstable():
    d = me.clarke_derivatives(L, B, T, CB)
    c = me.course_stability_discriminant(d)
    # Full-form tanker sits just below zero (needs corrective helm to hold course).
    assert c == pytest.approx(-9.3e-6, abs=2e-6)
    assert c < 0


# --- rudder lift slope ------------------------------------------------------

def test_lift_slope_with_hull_mirror():
    assert me.rudder_lift_slope_per_rad(A_R, SPAN, behind_hull=True) == pytest.approx(3.77, abs=0.02)


def test_lift_slope_no_mirror_is_lower():
    with_mirror = me.rudder_lift_slope_per_rad(A_R, SPAN, behind_hull=True)
    no_mirror = me.rudder_lift_slope_per_rad(A_R, SPAN, behind_hull=False)
    assert no_mirror < with_mirror
    assert no_mirror == pytest.approx(2.72, abs=0.03)


# --- turning circle + IMO compliance ---------------------------------------

def test_steady_radius_speed_independent():
    # R/L depends only on K' and delta, not speed.
    assert me.steady_turning_radius_over_L(1.02, 35.0) == pytest.approx(1.6, abs=0.05)


def test_tactical_diameter_and_imo_pass():
    td_over_l = me.tactical_diameter_over_L(1.02, 35.0)
    assert td_over_l == pytest.approx(3.2, abs=0.1)
    verdict = me.imo_turning_compliance(advance_m=720.0, tactical_diameter_m=td_over_l * L, lbp_m=L)
    assert verdict.tactical_diameter_pass  # 3.2L <= 5L
    assert verdict.advance_pass            # 3.2L <= 4.5L
    assert verdict.overall_pass


def test_imo_fails_when_circle_too_large():
    verdict = me.imo_turning_compliance(advance_m=6 * L, tactical_diameter_m=6 * L, lbp_m=L)
    assert not verdict.overall_pass


# --- threshold steerage speed ----------------------------------------------

def test_threshold_speed_laden_engine_off():
    u = me.threshold_speed_for_steerage(
        wind_speed_m_s=10.0, windage_area_m2=2200.0,
        rudder_effective_area_m2=44.94, rudder_span_m=9.0, inflow_factor=1.0,
    )
    assert u * me.M_PER_S_TO_KNOT == pytest.approx(2.9, abs=0.4)


def test_kick_ahead_lowers_threshold():
    kw = dict(wind_speed_m_s=10.0, windage_area_m2=3500.0,
              rudder_effective_area_m2=35.0, rudder_span_m=8.0)
    coasting = me.threshold_speed_for_steerage(inflow_factor=1.0, **kw)
    kick = me.threshold_speed_for_steerage(inflow_factor=1.5, **kw)
    assert kick < coasting
    assert kick * me.M_PER_S_TO_KNOT == pytest.approx(2.7, abs=0.4)


# --- OCIMF current moment + engine-on balance ------------------------------

def test_current_yaw_moment_beam_3kn():
    n = me.current_yaw_moment_ocimf(CXYc=0.05, current_speed_m_s=3.0 * me.KNOT_TO_M_PER_S,
                                    lbp_m=L, draft_m=T)
    assert n / 1e6 == pytest.approx(37.9, abs=1.0)


def test_current_moment_quadratic_in_speed():
    n1 = me.current_yaw_moment_ocimf(CXYc=0.05, current_speed_m_s=1.0, lbp_m=L, draft_m=T)
    n3 = me.current_yaw_moment_ocimf(CXYc=0.05, current_speed_m_s=3.0, lbp_m=L, draft_m=T)
    assert n3 / n1 == pytest.approx(9.0, rel=1e-6)


def _sirocco_propulsion():
    return VesselPropulsion(D=7.0, kt_coeffs=(0.35, -0.30), t=0.2, w=0.25, C_rp=0.5)


def test_engine_on_holds_near_head_current():
    vessel = _sirocco_propulsion()
    rudder = RudderGeometry(area=A_R, aspect_ratio=1.80, x_R=X_R)
    n_head = me.current_yaw_moment_ocimf(CXYc=0.012, current_speed_m_s=3.0 * me.KNOT_TO_M_PER_S,
                                         lbp_m=L, draft_m=T)
    res = me.rudder_angle_to_hold_heading(
        current_yaw_moment_Nm=n_head, ship_speed_m_s=1.0, shaft_speed_rev_s=1.2,
        vessel=vessel, rudder=rudder)
    assert res.can_hold_heading
    assert 0 < res.required_rudder_angle_deg < 35.0
    assert res.utilisation < 1.0


def test_beam_current_exceeds_rudder_authority():
    # At 5 kn beam current the yaw moment (~105 MN.m) is well beyond the
    # engine-on rudder authority (~39 MN.m at this thrust) — tug assist needed.
    vessel = _sirocco_propulsion()
    rudder = RudderGeometry(area=A_R, aspect_ratio=1.80, x_R=X_R)
    n_beam = me.current_yaw_moment_ocimf(CXYc=0.05, current_speed_m_s=5.0 * me.KNOT_TO_M_PER_S,
                                         lbp_m=L, draft_m=T)
    res = me.rudder_angle_to_hold_heading(
        current_yaw_moment_Nm=n_beam, ship_speed_m_s=1.0, shaft_speed_rev_s=1.2,
        vessel=vessel, rudder=rudder)
    assert not res.can_hold_heading
    assert res.required_rudder_angle_deg is None
    assert res.utilisation > 1.0


def test_critical_current_speed_positive_and_finite():
    vessel = _sirocco_propulsion()
    rudder = RudderGeometry(area=A_R, aspect_ratio=1.80, x_R=X_R)
    vc = me.critical_current_speed_for_heading(
        CXYc=0.012, lbp_m=L, draft_m=T, ship_speed_m_s=1.0, shaft_speed_rev_s=1.2,
        vessel=vessel, rudder=rudder)
    assert math.isfinite(vc) and vc > 0


def test_engine_off_moment_smaller_than_engine_on_authority():
    # Whicker-Fehlner engine-off yaw moment at 3 kn is tiny vs engine-on authority.
    n_off = me.engine_off_rudder_yaw_moment(
        current_speed_m_s=3.0 * me.KNOT_TO_M_PER_S, rudder_area_m2=A_R, rudder_span_m=SPAN,
        rudder_angle_deg=35.0, lever_arm_m=X_R)
    assert n_off / 1e6 < 20.0  # order ~13 MN.m
