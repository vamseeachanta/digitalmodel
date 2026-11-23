#!/usr/bin/env python3
"""
ABOUTME: Tests for the conduit fluid-inertia extension of the gravity-exchange
model (#1549 / #1528). The quasi-static ``simulate_gravity_exchange`` is the
zero-inertia (L_eff -> 0) limit; the inertial U-tube adds conduit momentum so the
system can oscillate and resonate (Frahm tuning), which the twin-tank CFD
confirmed. Oracles are analytic: the natural frequency, the steady-state orifice
limit, exact volume/mass conservation, flow reversal, and resonant amplification.
"""

import math

import pytest

from digitalmodel.solvers.openfoam.gravity_conduit import (
    G_STANDARD,
    ConduitGeometry,
    TankState,
    simulate_inertial_exchange,
    utube_natural_frequency,
)

# ---------------------------------------------------------------------------
# ConduitGeometry.effective_length
# ---------------------------------------------------------------------------


def test_effective_length_defaults_to_none_and_is_optional():
    conduit = ConduitGeometry(area=0.5)
    assert conduit.effective_length is None
    assert conduit.has_inertia is False


def test_effective_length_when_given_enables_inertia():
    conduit = ConduitGeometry(area=0.5, effective_length=12.0)
    assert conduit.effective_length == pytest.approx(12.0)
    assert conduit.has_inertia is True


@pytest.mark.parametrize("bad", [0.0, -3.0])
def test_effective_length_must_be_positive_when_given(bad):
    with pytest.raises(ValueError, match="effective_length"):
        ConduitGeometry(area=0.5, effective_length=bad)


# ---------------------------------------------------------------------------
# U-tube natural frequency
# ---------------------------------------------------------------------------


def test_utube_natural_frequency_matches_analytic_formula():
    # omega_tau = sqrt(2 g A_c / (L_eff A_t))
    conduit = ConduitGeometry(area=6.8, effective_length=12.0)
    tank_area = 120.0
    expected = math.sqrt(2.0 * G_STANDARD * 6.8 / (12.0 * 120.0))
    assert utube_natural_frequency(conduit, tank_area) == pytest.approx(expected)
    # sanity: this geometry tunes to a ~20 s period (matches the CFD Frahm band)
    period = 2.0 * math.pi / expected
    assert 18.0 < period < 23.0


def test_utube_natural_frequency_requires_effective_length():
    with pytest.raises(ValueError, match="effective_length"):
        utube_natural_frequency(ConduitGeometry(area=6.8), 120.0)


# ---------------------------------------------------------------------------
# simulate_inertial_exchange
# ---------------------------------------------------------------------------


def _level_tanks(vol, area=50.0, height=10.0, floor=0.0):
    return TankState(volume=vol, plan_area=area, height=height, floor_elevation=floor)


def test_inertial_requires_effective_length():
    src = _level_tanks(300.0)
    dst = _level_tanks(200.0)
    conduit = ConduitGeometry(area=0.5)  # no effective_length
    with pytest.raises(ValueError, match="effective_length"):
        simulate_inertial_exchange(src, dst, conduit, duration=5.0, dt=0.01)


def test_inertial_conserves_volume_and_mass_exactly():
    src = _level_tanks(320.0)
    dst = _level_tanks(180.0)
    conduit = ConduitGeometry(area=0.5, effective_length=8.0)
    res = simulate_inertial_exchange(src, dst, conduit, duration=30.0, dt=0.01)
    total0 = src.volume + dst.volume
    for vs, vd in zip(res.source_volume, res.dest_volume):
        assert vs + vd == pytest.approx(total0, abs=1e-9)
    assert res.mass_residual == pytest.approx(0.0, abs=1e-6)


def test_constant_head_asymptotes_to_orifice_flow():
    # Large equal tanks so geometric head barely moves; a fixed external head
    # forces the flow, which must relax to the quasi-static orifice value.
    big = 5000.0
    src = _level_tanks(25000.0, area=big, height=20.0)
    dst = _level_tanks(25000.0, area=big, height=20.0)
    conduit = ConduitGeometry(area=0.4, discharge_coefficient=0.6, effective_length=3.0)
    H = 2.0
    res = simulate_inertial_exchange(
        src, dst, conduit, duration=40.0, dt=0.005, external_head=lambda _t: H
    )
    q_steady = res.flow_rate[-1]
    cd_eff = conduit.effective_discharge_coefficient
    q_orifice = cd_eff * conduit.area * math.sqrt(2.0 * G_STANDARD * H)
    assert q_steady == pytest.approx(q_orifice, rel=2e-2)


def test_free_oscillation_period_matches_natural_frequency():
    # Release an out-of-balance U-tube with low loss; the oscillation period
    # must match 2*pi/omega_tau within a few percent (light damping).
    area = 40.0
    src = _level_tanks(260.0, area=area)  # +60 above the 200 balance
    dst = _level_tanks(140.0, area=area)  # -60
    conduit = ConduitGeometry(area=1.2, discharge_coefficient=1.0, effective_length=6.0)
    omega = utube_natural_frequency(conduit, area)
    period = 2.0 * math.pi / omega
    res = simulate_inertial_exchange(
        src, dst, conduit, duration=4.0 * period, dt=period / 400.0
    )
    # measure period from successive up-zero-crossings of the head
    t = res.time
    h = res.head
    crossings = [
        t[i] - (t[i] - t[i - 1]) * h[i] / (h[i] - h[i - 1])
        for i in range(1, len(h))
        if h[i - 1] < 0.0 <= h[i]
    ]
    assert len(crossings) >= 2
    measured = crossings[1] - crossings[0]
    assert measured == pytest.approx(period, rel=0.06)


def test_inertial_flow_reverses_under_oscillating_head():
    src = _level_tanks(250.0)
    dst = _level_tanks(250.0)
    conduit = ConduitGeometry(area=0.6, effective_length=5.0)
    res = simulate_inertial_exchange(
        src,
        dst,
        conduit,
        duration=25.0,
        dt=0.01,
        external_head=lambda tt: 1.5 * math.sin(2.0 * math.pi * tt / 8.0),
    )
    assert res.reversed_flow is True
    total0 = src.volume + dst.volume
    for vs, vd in zip(res.source_volume, res.dest_volume):
        assert vs + vd == pytest.approx(total0, abs=1e-9)


def test_resonant_forcing_amplifies_transfer():
    # Forcing at the U-tube natural frequency must move more liquid than forcing
    # well above it (the Frahm resonance the CFD sweep confirmed).
    area = 60.0
    conduit = ConduitGeometry(area=1.0, discharge_coefficient=0.9, effective_length=6.0)
    omega = utube_natural_frequency(conduit, area)

    def peak_transfer(drive_omega):
        src = _level_tanks(300.0, area=area)
        dst = _level_tanks(300.0, area=area)
        res = simulate_inertial_exchange(
            src,
            dst,
            conduit,
            duration=12.0 * (2.0 * math.pi / drive_omega),
            dt=(2.0 * math.pi / drive_omega) / 300.0,
            external_head=lambda tt: 0.5 * math.sin(drive_omega * tt),
        )
        return max(abs(v - 300.0) for v in res.source_volume)

    at_resonance = peak_transfer(omega)
    above = peak_transfer(3.0 * omega)
    assert at_resonance > 1.5 * above


def test_quadratic_loss_damps_free_oscillation():
    area = 40.0
    src = _level_tanks(260.0, area=area)
    dst = _level_tanks(140.0, area=area)
    # lower Cd => more loss => stronger decay
    conduit = ConduitGeometry(area=1.0, discharge_coefficient=0.5, effective_length=6.0)
    omega = utube_natural_frequency(conduit, area)
    period = 2.0 * math.pi / omega
    res = simulate_inertial_exchange(
        src, dst, conduit, duration=6.0 * period, dt=period / 400.0
    )
    # peak |head| in the first period must exceed that in the last period
    t = res.time
    h = [abs(x) for x in res.head]
    first = max(hh for tt, hh in zip(t, h) if tt <= period)
    last = max(hh for tt, hh in zip(t, h) if tt >= 5.0 * period)
    assert last < 0.7 * first
