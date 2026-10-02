"""Drift-off / drive-off vessel trajectory and the anti-recoil tension schedule of the event models (digitalmodel-data
#13, W414): pure functions checked against closed forms (synthetic, class-level inputs only)."""

from __future__ import annotations

import math

import pytest

from digitalmodel.drilling_riser.global_model import events as ev

G = 9.80665
VESSEL = ev.DriftVessel(displacement_t=50000.0, added_mass_surge=0.05, added_mass_sway=0.8, length_m=180.0,
                        beam_m=40.0, draft_m=10.0, wind_cx=0.8, wind_area_x_m2=1500.0, wind_cy=0.95,
                        wind_area_y_m2=4300.0, current_cx=0.04, current_cy=0.65)
# no hull drag: the closed forms of a constant force (still water still drags a moving hull in the full model)
FREE = ev.DriftVessel(**{**VESSEL.__dict__, "current_cx": 0.0, "current_cy": 0.0})


def test_constant_force_from_rest_follows_the_closed_form():
    env = ev.DriftEnvironment(heading_deg=0.0, wind_u10_m_s=20.0)
    tr = ev.drift_trajectory(FREE, env, duration_s=60.0, dt_s=0.01)
    f = 0.5 * ev.RHO_AIR * 20.0 ** 2 * 0.8 * 1500.0
    m = 50000.0e3 * 1.05
    for t, x in zip(tr["t"], tr["x"]):
        assert x == pytest.approx(0.5 * f / m * t * t, rel=2e-3, abs=1e-6)
    assert max(abs(y) for y in tr["y"]) < 1e-9


def test_beam_environment_drifts_in_sway_with_the_sway_added_mass():
    env = ev.DriftEnvironment(heading_deg=90.0, wind_u10_m_s=20.0)
    tr = ev.drift_trajectory(FREE, env, duration_s=30.0, dt_s=0.01)
    f = 0.5 * ev.RHO_AIR * 20.0 ** 2 * 0.95 * 4300.0
    assert tr["y"][-1] == pytest.approx(0.5 * f / (50000.0e3 * 1.8) * 30.0 ** 2, rel=2e-3)
    assert abs(tr["x"][-1]) < 1e-6


def test_current_drag_limits_the_drift_speed_to_the_current():
    env = ev.DriftEnvironment(heading_deg=90.0, current_m_s=1.0)
    tr = ev.drift_trajectory(VESSEL, env, duration_s=20000.0, dt_s=0.5)
    assert tr["vy"][-1] == pytest.approx(1.0, abs=0.01)  # the vessel tends to the current speed
    assert max(tr["vy"]) <= 1.0 + 1e-9  # never faster than the current


def test_wave_drift_force_is_the_reflection_limit_in_short_waves_and_falls_in_long_waves():
    short = ev.wave_drift_force_n(VESSEL, hs_m=4.0, tp_s=2.0, heading_deg=90.0)
    limit = ev.RHO_W * G * 4.0 ** 2 / 16.0 * VESSEL.length_m
    assert short[1] == pytest.approx(limit, rel=0.02) and abs(short[0]) < 1e-6
    long = ev.wave_drift_force_n(VESSEL, hs_m=4.0, tp_s=16.0, heading_deg=90.0)
    assert long[1] < 0.2 * short[1]


def test_drive_off_thrust_ramps_then_holds():
    env = ev.DriftEnvironment(heading_deg=0.0)
    tr = ev.drift_trajectory(FREE, env, duration_s=30.0, dt_s=0.01, thrust_n=4.05e6, thrust_ramp_s=15.0)
    m = 50000.0e3 * 1.05
    # x(15) = F t^3 / (6 M T_r) during the ramp
    i = tr["t"].index(15.0)
    assert tr["x"][i] == pytest.approx(4.05e6 * 15.0 ** 2 / (6 * m), rel=2e-3)


def test_initial_velocity_is_applied_along_the_environment():
    env = ev.DriftEnvironment(heading_deg=45.0)
    tr = ev.drift_trajectory(FREE, env, duration_s=10.0, dt_s=0.01, initial_speed_m_s=0.5)
    assert tr["x"][-1] == pytest.approx(5.0 * math.cos(math.radians(45)), rel=1e-6)
    assert tr["y"][-1] == pytest.approx(5.0 * math.sin(math.radians(45)), rel=1e-6)


def test_anti_recoil_schedule_closes_to_the_hold_tension():
    st = ev.anti_recoil_stages(t0_n=6.0e6, hold_n=3.0e6, closure_s=2.5, step_s=0.5)
    assert [s["duration_s"] for s in st[:-1]] == [0.5] * 5 and st[-1]["duration_s"] is None
    vals = [s["tension_n"] for s in st]
    assert vals[0] == pytest.approx(6.0e6 - 0.1 * 3.0e6)  # step midpoints of the linear closure
    assert vals[-1] == pytest.approx(3.0e6) and all(a > b for a, b in zip(vals, vals[1:]))
    failed = ev.anti_recoil_stages(t0_n=6.0e6, hold_n=3.0e6, closure_s=2.5, step_s=0.5, open_fraction=1 / 6)
    assert failed[-1]["tension_n"] == pytest.approx(5 / 6 * 3.0e6 + 1 / 6 * 6.0e6)
