# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent acoustic oracles; synthetic inputs, no vendor qualification."""

import math

import pytest

from digitalmodel.units import Q_
from digitalmodel.well.completion.pump_down import transients as tr
from digitalmodel.well.drilling.swab_surge import SwabSurge


def test_elastic_wave_speed_and_rigid_wall_limit():
    # Independently: compliance = (1 + K*D/(E*t))/K = 6e-10 /Pa.
    speed = tr.pipe_wave_speed(1000, 2e9, 0.1, 0.005, 200e9)
    assert speed.to("m/s").magnitude == pytest.approx(math.sqrt(1 / 6e-7))
    rigid = tr.pipe_wave_speed(1000, 2e9, 0.1, 0.005, 200e9, restraint_factor=0)
    assert rigid.to("m/s").magnitude == pytest.approx(math.sqrt(2e6))
    assert speed < rigid
    field = tr.pipe_wave_speed(
        Q_(1000, "kg/m**3").to("ppg"), Q_(2e9, "Pa").to("psi"),
        Q_(0.1, "m").to("inch"), Q_(0.005, "m").to("inch"),
        Q_(200e9, "Pa").to("psi"),
    )
    assert field.to("m/s").magnitude == pytest.approx(speed.magnitude)


def test_joukowsky_and_ramp_boundaries():
    rho, a, length = Q_(8.34, "ppg"), Q_(4400, "ft/s"), Q_(20000, "ft")
    tc = tr.critical_time(length, a)
    assert tc.to("s").magnitude == pytest.approx(100 / 11)
    dp = tr.surge_pressure(rho, a, Q_(1, "ft/s"), length)
    assert dp.to("psi").magnitude == pytest.approx(59.3, rel=0.001)
    for duration in (0, 4, tc):
        assert tr.ramp_attenuation(length, a, duration) == 1
    assert tr.ramp_attenuation(length, a, 2 * tc) == pytest.approx(0.5)
    assert tr.surge_pressure(rho, a, Q_(-1, "ft/s"), length) == -dp
    assert tr.surge_pressure(rho, a, 0, length).magnitude == 0
    assert tr.surge_pressure(rho, a, Q_(1, "ft/s"), length, 2 * tc) == dp / 2


@pytest.mark.parametrize("initial, final, seconds, pressure, force", [
    (12, 0, 0, 534.0, 9581.0),
    (16, 11, 3.9, 222.6, 3991.9),
])
def test_field_rate_steps(initial, final, seconds, pressure, force):
    area = Q_(math.pi * 4.778**2 / 4, "inch**2")
    result = tr.rate_step_transient(
        Q_(initial, "bbl/min"), Q_(final, "bbl/min"), area,
        Q_(8.34, "ppg"), Q_(4400, "ft/s"), Q_(20000, "ft"), seconds,
    )
    assert result.pressure_change.to("psi").magnitude == pytest.approx(pressure, rel=0.005)
    assert result.force_change.to("lbf").magnitude == pytest.approx(force, rel=0.005)
    assert result.attenuation == 1
    assert result.critical_time.to("s").magnitude == pytest.approx(100 / 11)
    # Rate-step conversion is checked against independent SI arithmetic.
    q_change = (initial - final) * 42 * 0.003785411784 / 60
    area_si = math.pi * (4.778 * 0.0254)**2 / 4
    rho_si = 8.34 * 0.45359237 / 0.003785411784
    expected_pa = rho_si * (4400 * 0.3048) * q_change / area_si
    assert result.pressure_change.to("Pa").magnitude == pytest.approx(expected_pa)
    assert result.velocity_change.to("m/s").magnitude == pytest.approx(q_change / area_si)
    si = tr.rate_step_transient(
        initial * 42 * 0.003785411784 / 60,
        final * 42 * 0.003785411784 / 60,
        area_si, rho_si, 4400 * 0.3048, 20000 * 0.3048, seconds,
    )
    assert si.pressure_change.magnitude == pytest.approx(
        result.pressure_change.magnitude, rel=1e-12,
    )


def test_maximum_rate_step_is_an_inverse_pressure_bound():
    # SI oracle: dp=1000*1000*(dQ/0.01), so 1e5 Pa permits 0.001 m3/s.
    args = (1e5, 1000, 1000, 0.01, 100)
    assert tr.maximum_rate_step(*args).to("m**3/s").magnitude == pytest.approx(0.001)
    assert tr.maximum_rate_step(*args, ramp_time=0.4).magnitude == pytest.approx(0.002)
    for duration in (0, 0.2, 0.4):
        limit = tr.maximum_rate_step(*args, ramp_time=duration)
        result = tr.rate_step_transient(limit, 0, 0.01, 1000, 1000, 100, duration)
        assert result.pressure_change.magnitude == pytest.approx(1e5)
        over = tr.rate_step_transient(1.01 * limit, 0, 0.01, 1000, 1000, 100, duration)
        assert over.pressure_change.magnitude > 1e5
    assert tr.maximum_rate_step(0, *args[1:]).magnitude == 0


def test_rate_step_sign_and_no_change():
    stop = tr.rate_step_transient(0.01, 0, 0.01, 1000, 1000, 100)
    start = tr.rate_step_transient(0, 0.01, 0.01, 1000, 1000, 100)
    assert start.pressure_change == -stop.pressure_change
    assert start.force_change == -stop.force_change
    constant = tr.rate_step_transient(0.01, 0.01, 0.01, 1000, 1000, 100)
    assert constant.pressure_change.magnitude == 0


@pytest.mark.parametrize("call", [
    lambda: tr.pipe_wave_speed(0, 2e9, 0.1, 0.005, 200e9),
    lambda: tr.pipe_wave_speed(1000, -1, 0.1, 0.005, 200e9),
    lambda: tr.pipe_wave_speed(1000, 2e9, 0, 0.005, 200e9),
    lambda: tr.pipe_wave_speed(1000, 2e9, 0.1, 0, 200e9),
    lambda: tr.pipe_wave_speed(1000, 2e9, 0.1, 0.005, float("inf")),
    lambda: tr.pipe_wave_speed(1000, 2e9, 0.1, 0.005, 200e9, restraint_factor=-1),
    lambda: tr.pipe_wave_speed(1000, 2e9, 0.1, 0.005, 200e9, restraint_factor=1.1),
    lambda: tr.critical_time(0, 1000),
    lambda: tr.critical_time(100, 0),
    lambda: tr.ramp_attenuation(100, 1000, -1),
    lambda: tr.ramp_attenuation(100, 1000, float("nan")),
    lambda: tr.surge_pressure(0, 1000, 1, 100),
    lambda: tr.surge_pressure(1000, 1000, float("nan"), 100),
    lambda: tr.maximum_rate_step(-1, 1000, 1000, 0.01, 100),
    lambda: tr.maximum_rate_step(1e5, 1000, 1000, 0, 100),
    lambda: tr.rate_step_transient(-1, 0, 0.01, 1000, 1000, 100),
    lambda: tr.rate_step_transient(0, float("inf"), 0.01, 1000, 1000, 100),
])
def test_invalid_inputs(call):
    with pytest.raises(ValueError):
        call()


@pytest.mark.parametrize("config, factor", [("closed", 13 / 16), ("open", 0.5)])
def test_swab_surge_acoustic_magnitude_and_reversal(config, factor):
    calc = SwabSurge(
        pipe_od=6, pipe_id=4, hole_diameter=10, mud_weight=8.34,
        tvd=1000, plastic_viscosity=20, yield_point=0, clinging_constant=0.25,
        pipe_config=config,
    )
    # Area-balance factors: closed 0.25+36/64; open 0.25+20/80.
    settings = dict(wave_speed_ft_s=4400, column_length_ft=20000)
    stop = calc.transient_pressure(120, 0, **settings)
    assert stop == pytest.approx(59.3 * 2 * factor, rel=0.001)
    assert calc.transient_pressure(0, 120, **settings) == stop
    assert calc.transient_pressure(-120, 0, **settings) == stop
    assert calc.transient_pressure(120, -120, **settings) == 2 * stop
    assert calc.transient_pressure(120, 120, **settings) == 0
    assert calc.transient_pressure(
        120, 0, **settings, ramp_time_s=200 / 11,
    ) == pytest.approx(stop / 2, rel=1e-12)
    # Wave-travel length is explicit; TVD is not substituted for measured length.
    assert calc.transient_pressure(120, 0, **settings, ramp_time_s=4) == stop
    with pytest.raises(ValueError):
        calc.transient_pressure(float("nan"), 0, **settings)
    with pytest.raises(ValueError):
        calc.transient_pressure(120, 0, **settings, ramp_time_s=-1)
