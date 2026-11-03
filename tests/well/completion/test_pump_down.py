"""Independent arithmetic and limiting cases for the steady conveyance screen."""

import math
from pathlib import Path

import pint
import pytest
import yaml

from digitalmodel.units import Q_
from digitalmodel.common.hydraulics import swamee_jain_friction_factor
from digitalmodel.well.completion.pump_down import rate_position_table
from digitalmodel.well.completion.pump_down.force_balance import (
    buoyancy_factor,
    cable_stretch,
    capstan_factor,
    critical_inclination,
    resisting_force,
    tension_profile,
)
from digitalmodel.well.completion.pump_down.friction import (
    annulus_hydraulic_diameter,
    darcy_weisbach_gradient,
    drag_reduction,
    dodge_metzner_factor,
    power_law_reynolds,
    reynolds_number,
)
from digitalmodel.well.completion.pump_down.geometry import (
    BypassGeometry,
    api_drift,
    bypass,
    runnable,
)
from digitalmodel.well.completion.pump_down.kinematics import (
    ideal_line_speed,
    solve_line_speed,
    stage_transit,
)
from digitalmodel.well.completion.pump_down.restriction import calibrate_k, restriction
from digitalmodel.well.tubulars.casing import casing

VECTORS = yaml.safe_load(Path(__file__).with_name("pump_down_vectors.yaml").read_text())


@pytest.mark.parametrize("v", VECTORS["restriction"])
def test_static_restriction(v):
    # Ac=pi*4.778^2/4=17.9300777253 in2; Q=15*42*231/1728/60 ft3/s.
    # rho=8.34*7.48051948 lbm/ft3; dp=K*rho*vb^2/(2*32.17404856*144).
    geometry = bypass(4.778, v["od_in"])
    result = restriction(Q_(15, "bbl/min"), Q_(0, "ft/s"), geometry, Q_(8.34, "ppg"))
    assert geometry.bypass_area.to("inch**2").magnitude == pytest.approx(v["area_in2"])
    assert geometry.beta == pytest.approx(v["beta"])
    assert result.bypass_velocity.to("ft/s").magnitude == pytest.approx(
        v["velocity_ft_s"]
    )
    assert result.pressure_drop.to("psi").magnitude == pytest.approx(v["dp_psi"])
    assert result.driving_force.to("lbf").magnitude == pytest.approx(v["force_lbf"])
    assert result.driving_force == result.pressure_drop * geometry.casing_area
    assert calibrate_k(
        result.pressure_drop, Q_(8.34, "ppg"), result.bypass_velocity
    ) == pytest.approx(2)


def test_force_scaling_and_moving_bypass():
    g = bypass(4.778, 4.5)
    base = restriction(Q_(15, "bbl/min"), Q_(0, "m/s"), g, Q_(8.34, "ppg"))
    twice = restriction(Q_(30, "bbl/min"), Q_(0, "m/s"), g, Q_(8.34, "ppg"))
    assert twice.driving_force / base.driving_force == pytest.approx(4)
    bare = bypass(4.778, 3.54)
    result = restriction(Q_(15, "bbl/min"), Q_(0, "m/s"), bare, Q_(8.34, "ppg"))
    assert result.driving_force / base.driving_force == pytest.approx(
        (g.beta / bare.beta) ** 2
    )
    moving = restriction(Q_(15, "bbl/min"), Q_(100, "ft/min"), g, Q_(8.34, "ppg"))
    assert moving.driving_force < base.driving_force


def test_resistance_and_slip_at_4900_ft():
    # 0.30*1.12*(873 + 0.229*4900)=670.3536 lbf; sigma=sqrt(Fres/Fstatic).
    resistance = resisting_force(Q_(873, "lbf"), Q_(229, "lbf/kft"), Q_(4900, "ft"), 90)
    assert resistance.to("lbf").magnitude == pytest.approx(VECTORS["resistance_lbf"])
    g = bypass(4.778, 4.5)
    speed = solve_line_speed(Q_(15, "bbl/min"), g, Q_(8.34, "ppg"), resistance)
    assert speed.slip_fraction == pytest.approx(0.528096, rel=1e-5)
    assert speed.line_speed.to("ft/min").magnitude == pytest.approx(319.18, abs=0.1)
    assert not speed.stall
    moving = restriction(Q_(15, "bbl/min"), speed.line_speed, g, Q_(8.34, "ppg"))
    assert moving.driving_force.to("lbf").magnitude == pytest.approx(670.3536)


def test_stall_exact_boundary_and_no_flow():
    g = bypass(4.778, 4.5)
    rho, q = Q_(8.34, "ppg"), Q_(15, "bbl/min")
    static = restriction(q, Q_(0, "m/s"), g, rho).driving_force
    for load in [static, 2 * static]:
        result = solve_line_speed(q, g, rho, load)
        assert (
            result.stall
            and result.line_speed.magnitude == 0
            and result.slip_fraction == 1
        )
    result = solve_line_speed(Q_(0, "bbl/min"), g, rho, Q_(0, "N"))
    assert result.stall and result.slip_fraction is None
    assert not solve_line_speed(q, g, rho, 0.999 * static).stall
    with pytest.raises(ValueError, match="gravity"):
        solve_line_speed(q, g, rho, Q_(-1, "N"))


def test_si_field_parity():
    field = bypass(4.778, 4.5)
    si = bypass(Q_(4.778, "inch").to("m"), Q_(4.5, "inch").to("m"))
    args = (Q_(15, "bbl/min"), Q_(0, "ft/s"), field, Q_(8.34, "ppg"))
    a = restriction(*args)
    b = restriction(args[0].to("m**3/s"), args[1].to("m/s"), si, args[3].to("kg/m**3"))
    assert a.driving_force.to("N").magnitude == pytest.approx(
        b.driving_force.to("N").magnitude
    )
    speed_a = solve_line_speed(args[0], field, args[3], Q_(670.3536, "lbf"))
    speed_b = solve_line_speed(
        args[0].to("m**3/s"), si, args[3].to("kg/m**3"), Q_(670.3536, "lbf").to("N")
    )
    assert speed_a.line_speed.to("m/s").magnitude == pytest.approx(
        speed_b.line_speed.to("m/s").magnitude
    )
    with pytest.raises(pint.DimensionalityError):
        restriction(Q_(15, "lbf"), args[1], field, args[3])


def test_water_friction_and_shared_helper():
    d, rho, viscosity = Q_(4.778, "inch"), Q_(8.34, "ppg"), Q_(1, "cP")
    v = Q_(15, "bbl/min") / bypass(4.778, 4.5).casing_area
    re = reynolds_number(rho, v, d, viscosity)
    assert re == pytest.approx(416726.7191)
    assert swamee_jain_friction_factor(re, 0.0018 / 4.778) == pytest.approx(
        VECTORS["water"]["darcy_factor"]
    )
    gradient = darcy_weisbach_gradient(rho, v, d, viscosity)
    # f_D*rho*v^2/(2*g_c*D*144)=0.03685 psi/ft. The 0.147 psi/ft target came
    # from the Fanning-form shortcut f*rho*v^2/(25.81*D) fed a Darcy factor.
    assert gradient.to("psi/ft").magnitude == pytest.approx(
        VECTORS["water"]["gradient_psi_ft"]
    )
    # Field shortcut with the Fanning factor agrees; a Darcy factor gives 4x.
    f_d, v_fps = VECTORS["water"]["darcy_factor"], v.to("ft/s").magnitude
    shortcut = 8.34 * v_fps**2 / (25.806 * 4.778)
    assert f_d / 4 * shortcut == pytest.approx(gradient.to("psi/ft").magnitude, rel=1e-3)
    assert f_d * shortcut == pytest.approx(0.147, abs=0.001)
    assert swamee_jain_friction_factor(1000, 0) == pytest.approx(0.064)
    assert darcy_weisbach_gradient(rho, Q_(0, "m/s"), d, viscosity).magnitude == 0
    reduced = darcy_weisbach_gradient(rho, v, d, viscosity, reduction=0.6)
    assert reduced / gradient == pytest.approx(0.4)
    assert annulus_hydraulic_diameter(d, Q_(3.54, "inch")).to(
        "inch"
    ).magnitude == pytest.approx(1.238)


def test_drag_reduction_and_power_law():
    assert drag_reduction(0, dr_max=0.8, c50=0.5) == 0
    assert drag_reduction(0.5, dr_max=0.8, c50=0.5) == pytest.approx(0.4)
    assert drag_reduction(1000, dr_max=0.8, c50=0.5) < 0.8
    rho, v, d, visc = Q_(1000, "kg/m**3"), Q_(2, "m/s"), Q_(0.1, "m"), Q_(0.001, "Pa*s")
    assert power_law_reynolds(rho, v, d, visc, 1) == pytest.approx(
        reynolds_number(rho, v, d, visc)
    )
    assert dodge_metzner_factor(1000, 0.6) == pytest.approx(0.064)
    for n in [0.4, 0.7, 1.0]:
        f_fanning = dodge_metzner_factor(20000, n) / 4
        # Original implicit equation, independently substituted; Darcy=4*Fanning.
        rhs = 4 / n**0.75 * math.log10(20000 * f_fanning ** (1 - n / 2)) - 0.4 / n**1.2
        assert 1 / math.sqrt(f_fanning) == pytest.approx(rhs)


@pytest.mark.parametrize("mu, angle", [(0.2, 78.7), (0.3, 73.3), (0.4, 68.2)])
def test_critical_inclination(mu, angle):
    assert critical_inclination(mu) == pytest.approx(angle, abs=0.05)
    assert resisting_force(
        Q_(100, "lbf"),
        Q_(0, "lbf/ft"),
        Q_(0, "ft"),
        critical_inclination(mu),
        mu=mu,
        eccentricity=0,
    ).magnitude == pytest.approx(0, abs=1e-10)


def survey(depths, incs, azis=None):
    return [
        dict(measured_depth_m=d, inclination_deg=i, azimuth_deg=a)
        for d, i, a in zip(depths, incs, azis or [0] * len(depths))
    ]


def test_vertical_profile_and_stretch_integral():
    profile = tension_profile(
        survey([0, 1000], [0, 0]), Q_(100, "N"), Q_(2, "N/m"), mu=0.3
    )
    assert profile[0].tension.to("N").magnitude == pytest.approx(2100)
    assert profile[-1].tension.to("N").magnitude == pytest.approx(100)
    # Integral (100+2*(1000-s)) ds / 1e7 = 0.11 m; surface-only gives 0.21 m.
    assert cable_stretch(profile, Q_(1e7, "N")).to("m").magnitude == pytest.approx(0.11)


def test_capstan_direction_azimuth_and_refinement():
    assert capstan_factor(0.3, 90) == pytest.approx(1.60197765)
    for direction, sign in [("pickup", 1), ("payout", -1)]:
        coarse = tension_profile(
            survey([0, 100], [90, 90], [0, 90]),
            Q_(1000, "N"),
            Q_(0, "N/m"),
            mu=0.3,
            direction=direction,
        )
        fine = tension_profile(
            survey([0, 50, 100], [90] * 3, [0, 45, 90]),
            Q_(1000, "N"),
            Q_(0, "N/m"),
            mu=0.3,
            direction=direction,
        )
        assert coarse[0].tension.to("N").magnitude == pytest.approx(
            1000 * math.exp(sign * 0.3 * math.pi / 2)
        )
        assert coarse[0].tension == fine[0].tension
        # Analytical integral along a constant-curvature, weightless cable.
        expected = (
            1000
            * 100
            * math.expm1(sign * 0.3 * math.pi / 2)
            / (sign * 0.3 * math.pi / 2)
            / 1e7
        )
        assert cable_stretch(coarse, Q_(1e7, "N")).to("m").magnitude == pytest.approx(
            expected
        )


def test_payout_slack_is_not_reported_as_compression():
    with pytest.raises(ValueError, match="slack"):
        tension_profile(survey([0, 100], [90, 90]), Q_(0, "N"), Q_(1, "N/m"), mu=0.3)


@pytest.mark.parametrize("direction, sign", [("pickup", 1), ("payout", -1)])
def test_weighted_quarter_circle_against_exact_solution(direction, sign):
    # Constant curvature k=pi/(2L), upward x from toe: theta=pi/2-k*x.
    # dT/dx=w*sin(k*x)+sign*mu*w*cos(k*x)+sign*mu*k*T.
    # Exact T(x)=(Ttoe+w/k)*exp(sign*mu*k*x)-w/k*cos(k*x).
    length, weight, toe, mu = 100, 2, 1000, 0.3
    curvature = math.pi / (2 * length)
    exponent = sign * mu * math.pi / 2
    expected_surface = (toe + weight / curvature) * math.exp(exponent)
    expected_integral = (toe + weight / curvature) * length * math.expm1(
        exponent
    ) / exponent - weight / curvature**2
    profile = tension_profile(
        survey([0, length], [0, 90]),
        Q_(toe, "N"),
        Q_(weight, "N/m"),
        mu=mu,
        direction=direction,
        max_turn_deg=0.1,
    )
    assert profile[0].tension.to("N").magnitude == pytest.approx(
        expected_surface, rel=1e-6
    )
    assert cable_stretch(profile, Q_(1e7, "N")).to("m").magnitude == pytest.approx(
        expected_integral / 1e7, rel=1e-6
    )


def test_direct_geometry_construction_cannot_bypass_validation():
    with pytest.raises(ValueError):
        BypassGeometry(Q_(1, "m**2"), Q_(2, "m**2"), Q_(-1, "m**2"), -1)
    with pytest.raises(ValueError):
        BypassGeometry(Q_(1, "m**2"), Q_(0.5, "m**2"), Q_(0.4, "m**2"), 0.4)


@pytest.mark.parametrize("od, weight, drift", VECTORS["drift"])
def test_six_public_drift_values(od, weight, drift):
    product = casing("P110", od_in=od, weight_ppf=weight)
    assert product.drift_in == pytest.approx(drift)
    assert api_drift(product.id_in) == pytest.approx(drift)


def test_geometry_limits_and_drift_clearance():
    assert not runnable(4.5, api_drift(4.778))
    assert runnable(4.45, 4.653)
    assert runnable(4.5, 4.7)
    assert buoyancy_factor(Q_(8.34, "ppg")) == pytest.approx(1 - 8.34 / 65.4)
    for d, od in [(0, 1), (4, 4), (4, 5), (float("nan"), 1), (4, -1)]:
        with pytest.raises(ValueError):
            bypass(d, od)
    assert bypass(4, 0).beta == 1


def test_drift_override_and_unknown_large_casing():
    assert casing("P110", od_in=9.625, weight_ppf=47).drift_in is None
    assert casing("P110", od_in=9.625, weight_ppf=47, drift_in=8.525).drift_in == 8.525
    with pytest.raises(ValueError):
        casing("P110", od_in=5.5, weight_ppf=20, drift_in=5)


def test_stage_time_volume_and_ideal_speed():
    assert ideal_line_speed(15, 4.778) == pytest.approx(1029.4 * 15 / 4.778**2)
    result = stage_transit(Q_(1000, "ft"), Q_(320, "ft/min"), Q_(15, "bbl/min"))
    assert result.time.to("min").magnitude == pytest.approx(3.125)
    assert result.volume.to("bbl").magnitude == pytest.approx(46.875)
    stalled = stage_transit(Q_(1000, "ft"), Q_(0, "ft/min"), Q_(15, "bbl/min"))
    assert math.isinf(stalled.time.magnitude) and stalled.volume is None
    assert stage_transit(Q_(0, "ft"), Q_(0, "m/s"), Q_(0, "m**3/s")).time.magnitude == 0


def test_table_has_each_rate_and_position_and_consistent_loads():
    rows = rate_position_table(
        rates=[Q_(5, "bbl/min"), Q_(15, "bbl/min")],
        lateral_positions=[Q_(2450, "ft"), Q_(4900, "ft")],
        geometry=bypass(4.778, 4.5),
        density=Q_(8.34, "ppg"),
        buoyed_bha_weight=Q_(873, "lbf"),
        buoyed_cable_weight=Q_(229, "lbf/kft"),
        vertical_length=Q_(9500, "ft"),
        cable_axial_stiffness=Q_(100000, "lbf"),
        weak_point=Q_(4000, "lbf"),
    )
    assert len(rows) == 4
    for row in rows:
        for key in [
            "beta",
            "bypass_velocity",
            "pressure_drop",
            "driving_force",
            "resisting_force",
            "line_speed",
            "slip_fraction",
            "stall",
            "surface_tension",
            "weak_point_margin",
            "stretch",
            "citations",
        ]:
            assert key in row
        expected_margin = (Q_(4000, "lbf") - row["max_tension"]).to("N").magnitude
        assert row["weak_point_margin"].to("N").magnitude == pytest.approx(
            expected_margin
        )
        if not row["stall"]:
            assert row["driving_force"].to("lbf").magnitude == pytest.approx(
                row["resisting_force"].to("lbf").magnitude
            )
        assert row["surface_tension"].to("lbf").magnitude == pytest.approx(9500 * 0.229)
    assert rows[0]["stall"] and not rows[-1]["stall"]
    assert rows[-1]["slip_fraction"] == pytest.approx(0.528096, rel=1e-5)


@pytest.mark.parametrize(
    "call",
    [
        lambda: api_drift(0.1),
        lambda: runnable(1, 2, min_radial=-1),
        lambda: drag_reduction(-1, dr_max=0.8, c50=1),
        lambda: drag_reduction(1, dr_max=1.2, c50=1),
        lambda: dodge_metzner_factor(20000, 0),
        lambda: swamee_jain_friction_factor(1e5, -0.1),
        lambda: restriction(
            Q_(-1, "m**3/s"), Q_(0, "m/s"), bypass(4.778, 4.5), Q_(1000, "kg/m**3")
        ),
        lambda: calibrate_k(Q_(1, "Pa"), Q_(1000, "kg/m**3"), Q_(0, "m/s")),
        lambda: tension_profile(survey([0, 0], [0, 90]), Q_(1, "N"), Q_(1, "N/m")),
        lambda: tension_profile(survey([1, 10], [0, 90]), Q_(1, "N"), Q_(1, "N/m")),
        lambda: stage_transit(Q_(-1, "m"), Q_(1, "m/s"), Q_(1, "m**3/s")),
    ],
)
def test_invalid_inputs(call):
    with pytest.raises(ValueError):
        call()
