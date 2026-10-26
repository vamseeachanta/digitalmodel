"""Derived governing-quantity tests (digitalmodel #1359).

Uses constant/linear motions so numerical derivatives are exact, and verifies
governing series are non-negative magnitudes (the sign guard).
"""

import numpy as np
import pytest

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


@pytest.mark.parametrize(
    "governing",
    [
        "crane_tip_vertical_velocity",
        "gangway_tip_vertical_velocity",
        "top_connection_vertical_velocity",
    ],
)
@pytest.mark.parametrize("heave_rate", [0.05, 0.2], ids=["downward", "upward"])
def test_combined_axis_point_velocity_hand_calculation(governing, heave_rate):
    """Independent rigid-body hand calculation for synthetic linear traces.

    Let tau = t - 20 s, h = 1.5 + heave_rate*tau m,
    roll = -1 + 2*tau deg, pitch = 0.5 + 3*tau deg, rx = 4 m, ry = 3 m.
    With k = pi/180 rad/deg, z = h + ry*roll_rad - rx*pitch_rad becomes
    z = (1.5 - 5*k) + (heave_rate - 6*k)*tau m.
    Thus dz/dt = heave_rate - pi/30 m/s: negative for 0.05 m/s heave,
    positive for 0.2 m/s heave. Neither rotational sign can be reversed
    without changing the expected magnitude because heave is nonzero.
    """
    # Synthetic shifted, nonuniform grid in seconds, including both endpoints.
    t = np.array([20.0, 20.125, 20.5, 21.0, 22.0])
    tau = t - t[0]
    m = _motion(
        t,
        heave=1.5 + heave_rate * tau,
        roll=-1.0 + 2.0 * tau,
        pitch=0.5 + 3.0 * tau,
    )
    offset = (4.0, 3.0, 0.0)  # Hand-calculation lever arms; rz has no z term.
    expected = abs(heave_rate - np.pi / 30.0)
    # Linear derivatives are exact analytically; allow only floating roundoff.
    np.testing.assert_allclose(
        vertical_velocity_magnitude(m, offset), expected, rtol=0.0, atol=1e-12
    )

    series = compute_governing(m, governing, offset)
    assert series.name == governing
    assert series.unit == "m/s"
    np.testing.assert_array_equal(series.t, t)
    np.testing.assert_allclose(series.values, expected, rtol=0.0, atol=1e-12)
    assert np.all(series.values >= 0.0)


@pytest.mark.parametrize(
    "governing",
    [
        "crane_tip_vertical_velocity",
        "gangway_tip_vertical_velocity",
        "top_connection_vertical_velocity",
    ],
)
def test_rotations_cancel_heave_velocity(governing):
    """Synthetic hand calculation: nonzero axis velocities sum to zero.

    For tau = t - 20 s, choose h = 1/2 + (5/32)*tau m,
    roll_rad = 1/64 + tau/32, pitch_rad = -1/128 + tau/16,
    rx = 4 m and ry = 3 m. Then z = h + 3*roll_rad - 4*pitch_rad
    = 37/64 + (5/32 + 3/32 - 4/16)*tau = 37/64 m,
    so dz/dt = 0 m/s despite all three axes moving. Angles are stored
    in degrees; binary fractions make the cancellation exact analytically.
    """
    t = np.array([20.0, 20.125, 20.5, 21.0, 22.0])
    tau = t - t[0]
    m = _motion(
        t,
        heave=0.5 + (5.0 / 32.0) * tau,
        roll=np.rad2deg(1.0 / 64.0 + tau / 32.0),
        pitch=np.rad2deg(-1.0 / 128.0 + tau / 16.0),
    )
    offset = (4.0, 3.0, 0.0)  # Lever arms from the hand calculation above.
    # Zero analytic velocity; tolerate only degree-conversion roundoff.
    np.testing.assert_allclose(
        vertical_velocity_magnitude(m, offset), 0.0, rtol=0.0, atol=1e-12
    )

    series = compute_governing(m, governing, offset)
    assert series.name == governing
    assert series.unit == "m/s"
    np.testing.assert_array_equal(series.t, t)
    np.testing.assert_allclose(series.values, 0.0, rtol=0.0, atol=1e-12)
    assert np.all(series.values >= 0.0)


def test_compute_governing_dispatch_and_unknown():
    t = np.linspace(0, 10, 51)
    m = _motion(t, heave=0.5 * t)
    gs = compute_governing(m, "helideck_heave_velocity")
    assert gs.unit == "m/s" and np.allclose(gs.values, 0.5, atol=1e-9)
    with pytest.raises(ValueError, match="unknown governing"):
        compute_governing(m, "nope")
