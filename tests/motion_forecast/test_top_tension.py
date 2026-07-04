"""Lumped SDOF top-tension screen tests (digitalmodel #1359 fast-follow).

The Newmark integrator is validated against the closed-form base-excitation
transmissibilities (displacement and force/tension).
"""

import numpy as np
import pytest

from digitalmodel.motion_forecast.models import DOF_NAMES, MotionForecast
from digitalmodel.motion_forecast.top_tension import (
    LumpedLine,
    displacement_transmissibility,
    dynamic_tension,
    force_transmissibility,
)
from digitalmodel.motion_forecast.top_tension import _newmark


def _line(static_tension=1.0e5, zeta=0.1):
    # m = 10000 kg, k = 10000 N/m -> wn = 1.0 rad/s
    return LumpedLine(payload_mass=5000.0, added_mass=5000.0, ea=1.0e6,
                      length=100.0, damping_ratio=zeta, static_tension=static_tension)


def _heave_motion(A, w, t):
    dof = {d: np.zeros_like(t) for d in DOF_NAMES}
    dof["heave"] = A * np.cos(w * t)
    return MotionForecast(t=t, dof=dof, origin_time=float(t[0]),
                          horizon=float(t[-1] - t[0]))


def _tail_amp(x):
    tail = x[x.size // 2:]
    return 0.5 * (tail.max() - tail.min())


def test_line_derived_properties():
    ln = _line()
    assert ln.m == 10000.0 and ln.k == pytest.approx(1.0e4)
    assert ln.wn == pytest.approx(1.0)
    assert ln.c == pytest.approx(2 * 0.1 * np.sqrt(1e4 * 1e4))


def test_displacement_transmissibility_at_resonance():
    ln = _line(zeta=0.1)
    # base-excitation value sqrt(1+4 zeta^2)/(2 zeta), NOT 1/(2 zeta)
    assert displacement_transmissibility(ln.wn, ln) == pytest.approx(
        np.sqrt(1 + 4 * 0.1**2) / (2 * 0.1), rel=1e-9)


def test_force_transmissibility_is_m_w2_times_displacement():
    ln = _line()
    w = 0.7
    assert force_transmissibility(w, ln) == pytest.approx(
        ln.m * w * w * displacement_transmissibility(w, ln), rel=1e-12)


def test_newmark_matches_displacement_transmissibility():
    ln = _line()
    w, A = 0.5, 0.2
    t = np.arange(0.0, 200.0, 0.05)
    z = A * np.cos(w * t)
    zdot = -A * w * np.sin(w * t)
    x, _v = _newmark(z, zdot, t[1] - t[0], ln)
    assert _tail_amp(x) == pytest.approx(displacement_transmissibility(w, ln) * A, rel=0.02)


def test_tension_amplitude_matches_force_transmissibility():
    ln = _line()
    w, A = 0.5, 0.2
    t = np.arange(0.0, 200.0, 0.1)
    res = dynamic_tension(_heave_motion(A, w, t), ln)
    amp = _tail_amp(res.tension)
    assert amp == pytest.approx(force_transmissibility(w, ln) * A, rel=0.02)
    assert not res.snatch and res.daf_valid
    assert res.daf == pytest.approx(1.0 + amp / ln.static_tension, rel=0.02)


def test_static_limit_daf_near_one_no_snatch():
    ln = _line()
    t = np.arange(0.0, 2000.0, 1.0)
    res = dynamic_tension(_heave_motion(0.1, 0.01, t), ln)  # very slow base motion
    assert res.daf == pytest.approx(1.0, abs=5e-3)
    assert not res.snatch


def test_large_near_resonance_triggers_snatch():
    ln = _line(static_tension=1.0e5)
    t = np.arange(0.0, 200.0, 0.1)
    res = dynamic_tension(_heave_motion(3.0, 1.0, t), ln)  # A=3 at resonance
    assert res.snatch and not res.daf_valid
    assert res.min_tension <= 0.0


@pytest.mark.parametrize("kw", [
    {"payload_mass": 0.0}, {"ea": -1.0}, {"length": 0.0},
    {"static_tension": 0.0}, {"damping_ratio": 1.0}, {"damping_ratio": 0.0},
])
def test_invalid_line_raises(kw):
    base = dict(payload_mass=5000.0, added_mass=5000.0, ea=1.0e6,
                length=100.0, damping_ratio=0.1, static_tension=1.0e5)
    base.update(kw)
    with pytest.raises(ValueError):
        LumpedLine(**base)
