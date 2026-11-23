"""Reconstruction core tests (digitalmodel #1358).

Regular-wave analytic checks are the *sole phase guard* (the irregular
cross-check in test_validation is phase-blind), so they verify amplitude AND
signed phase, including the incident-wave spatial-phase transfer term.
"""

from unittest.mock import Mock

import numpy as np
import pytest

from digitalmodel.motion_forecast import (
    AnalyticRAO,
    WaveComponent,
    WaveForecast,
    reconstruct_motion,
    vertical_motion_at,
    wavenumber,
)
from digitalmodel.motion_forecast.models import GRAVITY


def _const_rao(mag, ang):
    return AnalyticRAO({"heave": lambda w, b: mag * np.exp(1j * ang)})


def test_regular_wave_amplitude_and_phase():
    w0, a0, phi0 = 0.7, 1.3, 0.4
    mag, ang = 0.8, 0.5
    fc = WaveForecast([WaveComponent(w0, a0, phi0, heading=0.0)], horizon=60.0)
    mf = reconstruct_motion(fc, _const_rao(mag, ang), dt=0.1)
    expected = a0 * mag * np.cos(w0 * mf.t + phi0 + ang)
    assert np.allclose(mf.dof["heave"], expected, atol=1e-6)


def test_spatial_phase_transfer_term():
    """Moving the asset downwave shifts the phase by -k*dx (dispersion)."""
    w0, a0, phi0 = 0.9, 1.0, 0.0
    mag, ang = 1.0, 0.0
    Lx = 120.0
    fc = WaveForecast(
        [WaveComponent(w0, a0, phi0, heading=0.0)],
        horizon=60.0, phase_reference_location=(0.0, 0.0),
    )
    mf = reconstruct_motion(fc, _const_rao(mag, ang), asset_location=(Lx, 0.0), dt=0.1)
    k = w0 * w0 / GRAVITY
    expected = a0 * mag * np.cos(w0 * mf.t + phi0 - k * Lx + ang)
    assert np.allclose(mf.dof["heave"], expected, atol=1e-6)


def test_heading_projects_offset():
    """A beam-going wave (heading 90) is unaffected by an along-x offset."""
    w0 = 0.8
    fc = WaveForecast([WaveComponent(w0, 1.0, 0.0, heading=90.0)], horizon=40.0)
    base = reconstruct_motion(fc, _const_rao(1.0, 0.0), asset_location=(0.0, 0.0), dt=0.2)
    shifted = reconstruct_motion(fc, _const_rao(1.0, 0.0), asset_location=(200.0, 0.0), dt=0.2)
    # propagation is +y; x-offset has zero projection -> identical series
    assert np.allclose(base.dof["heave"], shifted.dof["heave"], atol=1e-9)


def test_horizon_fail_closed():
    fc = WaveForecast([WaveComponent(0.7, 1.0, 0.0)], horizon=30.0, origin_time=0.0)
    with pytest.raises(ValueError, match="predictable"):
        reconstruct_motion(fc, _const_rao(1.0, 0.0), t_grid=np.linspace(0.0, 45.0, 10))


def _time_forecast(**overrides):
    """Hand-calculated case: 2*cos(pi*t/2), predictable zone [4, 8] s.

    A unit transfer, amplitude 2 m and period 4 s give heave values
    2, 0, -2, 2 m at times 4, 5, 6, 8 s respectively. These synthetic
    values isolate the time-input contract of issue #1358 from RAO physics.
    """
    fields = {"origin_time": 4.0, "horizon": 4.0}
    fields.update(overrides)
    return WaveForecast([WaveComponent(np.pi / 2.0, 2.0, 0.0)], **fields)


@pytest.fixture
def unused_rao():
    """Reject any transfer call before invalid time input has been rejected."""
    rao = Mock()
    rao.H.side_effect = AssertionError("invalid time input reached RAO transfer")
    return rao


@pytest.mark.parametrize("t_grid", [None, [4.0, 5.0, 8.0]])
@pytest.mark.parametrize(
    "field, value",
    [("origin_time", value) for value in (np.nan, np.inf, -np.inf)]
    + [("horizon", value) for value in (np.nan, np.inf, -np.inf, 0.0, -1.0)],
)
def test_invalid_predictable_zone_bounds(field, value, t_grid, unused_rao):
    """Issue #1358 requires known finite bounds and a positive zone length."""
    with pytest.raises(ValueError, match=field):
        reconstruct_motion(_time_forecast(**{field: value}), unused_rao, t_grid=t_grid)
    unused_rao.H.assert_not_called()


@pytest.mark.parametrize("t_grid", [None, [4.0]])
def test_predictable_zone_endpoint_must_be_finite(t_grid, unused_rao):
    """Two maximum finite floats overflow their sum: the endpoint is unknown."""
    largest = float(np.finfo(float).max)
    fc = _time_forecast(origin_time=largest, horizon=largest)
    with pytest.raises(ValueError, match="origin_time.*horizon"):
        reconstruct_motion(fc, unused_rao, t_grid=t_grid)
    unused_rao.H.assert_not_called()


@pytest.mark.parametrize("dt", [np.nan, np.inf, -np.inf, 0.0, -1.0])
def test_invalid_default_time_step(dt, unused_rao):
    """A finite positive step is required to construct the default time grid."""
    with pytest.raises(ValueError, match="dt"):
        reconstruct_motion(_time_forecast(), unused_rao, dt=dt)
    unused_rao.H.assert_not_called()


@pytest.mark.parametrize(
    "t_grid",
    [
        [np.nan], [4.0, np.nan, 8.0], [4.0, np.inf], [-np.inf, 8.0],
        [], [[4.0, 8.0]], [[], []], 4.0, [[4.0], [5.0, 8.0]], ["invalid"],
    ],
)
def test_invalid_explicit_time_grid(t_grid, unused_rao):
    """Issue #1358 requires a nonempty finite one-dimensional time sequence."""
    with pytest.raises(ValueError, match="t_grid"):
        reconstruct_motion(_time_forecast(), unused_rao, t_grid=t_grid)
    unused_rao.H.assert_not_called()


@pytest.mark.parametrize(
    "t_grid, expected",
    [
        ([4.0], [2.0]), ([8.0], [2.0]),
        ([4.0, 8.0], [2.0, 2.0]), ([4.0, 5.0, 8.0], [2.0, 0.0, 2.0]),
        # Existing reconstruct_motion tolerance is 1e-9 s; half stays inside.
        ([4.0 - 0.5e-9, 8.0 + 0.5e-9], [2.0, 2.0]),
    ],
)
def test_valid_explicit_time_grid(t_grid, expected):
    """Preserve endpoints, single times and nonuniform hand-calculated motion."""
    mf = reconstruct_motion(_time_forecast(), _const_rao(1.0, 0.0), t_grid=t_grid)
    np.testing.assert_array_equal(mf.t, t_grid)
    # Roundoff allowance for the hand-calculated zeros of 2*cos(pi*t/2).
    np.testing.assert_allclose(mf.dof["heave"], expected, atol=1e-14)


def test_valid_default_time_grid():
    """The hand calculation above sampled every 2 s gives [2, -2, 2] m."""
    mf = reconstruct_motion(_time_forecast(), _const_rao(1.0, 0.0), dt=2.0)
    np.testing.assert_array_equal(mf.t, [4.0, 6.0, 8.0])
    np.testing.assert_allclose(mf.dof["heave"], [2.0, -2.0, 2.0])


def test_explicit_time_grid_does_not_use_default_time_step():
    """An explicit time grid does not use dt to construct sample times."""
    mf = reconstruct_motion(
        _time_forecast(), _const_rao(1.0, 0.0), t_grid=[4.0], dt=np.nan,
    )
    np.testing.assert_allclose(mf.dof["heave"], [2.0])


@pytest.mark.parametrize(
    "t_grid",
    [
        [3.0, 4.0], [8.0, 9.0],
        # Twice the existing 1e-9 s endpoint tolerance lies outside the zone.
        [4.0 - 2e-9, 8.0], [4.0, 8.0 + 2e-9],
    ],
)
def test_outside_predictable_zone_before_transfer(t_grid, unused_rao):
    """Reject both sides of the zone before invoking the synthetic transfer."""
    with pytest.raises(ValueError, match="t_grid.*predictable"):
        reconstruct_motion(_time_forecast(), unused_rao, t_grid=t_grid)
    unused_rao.H.assert_not_called()


def test_finite_depth_wavenumber_reduces_to_deep():
    w = 0.9
    assert wavenumber(w, None) == pytest.approx(w * w / GRAVITY)
    # deep-water limit: large depth -> tanh -> 1
    assert wavenumber(w, 100000.0) == pytest.approx(w * w / GRAVITY, rel=1e-6)


@pytest.mark.parametrize("w", [0.3, 0.6, 0.9, 1.4])
@pytest.mark.parametrize("h", [2.0, 5.0, 10.0, 40.0])
def test_finite_depth_dispersion_holds_in_shallow_water(w, h):
    """The solver must satisfy omega^2 = g k tanh(k h) even in shallow water
    (the old fixed-point iteration diverged and returned a wrong k silently)."""
    k = wavenumber(w, h)
    assert k > 0
    assert w * w == pytest.approx(GRAVITY * k * np.tanh(k * h), rel=1e-6)
    # finite depth shortens waves: k >= deep-water k
    assert k >= w * w / GRAVITY - 1e-12


def test_vertical_motion_lever_arm():
    """z at (rx,0,0) = heave - rx*pitch(rad)."""
    from digitalmodel.motion_forecast.models import MotionForecast

    t = np.linspace(0, 10, 51)
    heave = np.sin(t)
    pitch_deg = 3.0 * np.cos(t)  # degrees
    mf = MotionForecast(
        t=t,
        dof={"heave": heave, "pitch": pitch_deg, "roll": np.zeros_like(t),
             "surge": np.zeros_like(t), "sway": np.zeros_like(t), "yaw": np.zeros_like(t)},
        origin_time=0.0, horizon=10.0,
    )
    rx = 40.0
    z = vertical_motion_at(mf, (rx, 0.0, 0.0))
    assert np.allclose(z, heave - rx * np.deg2rad(pitch_deg), atol=1e-9)
