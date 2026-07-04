"""Derived governing-quantity tests (digitalmodel #1359).

Uses constant/linear motions so numerical derivatives are exact, and verifies
governing series are non-negative magnitudes (the sign guard).
"""

import numpy as np

from digitalmodel.motion_forecast.derived import (
    compute_governing,
    heave_velocity_magnitude,
    inclination_deg,
    vertical_excursion_magnitude,
    vertical_velocity_magnitude,
)
from digitalmodel.motion_forecast.models import MotionForecast


def _motion(t, **dofs):
    base = {d: np.zeros_like(t) for d in
            ("surge", "sway", "heave", "roll", "pitch", "yaw")}
    base.update(dofs)
    return MotionForecast(t=t, dof=base, origin_time=float(t[0]),
                          horizon=float(t[-1] - t[0]))


def test_inclination_is_resultant_angle():
    t = np.linspace(0, 10, 51)
    m = _motion(t, roll=np.full_like(t, 3.0), pitch=np.full_like(t, 4.0))
    assert np.allclose(inclination_deg(m), 5.0)  # hypot(3,4)


def test_excursion_includes_lever_arm():
    t = np.linspace(0, 10, 51)
    m = _motion(t, heave=np.full_like(t, 2.0), pitch=np.full_like(t, 1.0))
    rx = 10.0
    expected = abs(2.0 - rx * np.deg2rad(1.0))
    assert np.allclose(vertical_excursion_magnitude(m, (rx, 0.0, 0.0)), expected)


def test_velocity_magnitude_of_linear_motion_is_constant():
    t = np.linspace(0, 10, 101)
    m = _motion(t, heave=0.5 * t)  # dz/dt = 0.5
    v = vertical_velocity_magnitude(m, (0.0, 0.0, 0.0))
    assert np.allclose(v, 0.5, atol=1e-9)
    assert np.all(v >= 0.0)


def test_velocity_magnitude_is_positive_for_downward_motion():
    """Sign guard: a downward (negative) velocity yields a positive magnitude."""
    t = np.linspace(0, 10, 101)
    m = _motion(t, heave=-0.5 * t)  # dz/dt = -0.5
    assert np.allclose(heave_velocity_magnitude(m), 0.5, atol=1e-9)


def test_compute_governing_dispatch_and_unknown():
    t = np.linspace(0, 10, 51)
    m = _motion(t, heave=0.5 * t)
    gs = compute_governing(m, "helideck_heave_velocity")
    assert gs.unit == "m/s" and np.allclose(gs.values, 0.5, atol=1e-9)
    import pytest
    with pytest.raises(ValueError, match="unknown governing"):
        compute_governing(m, "nope")
