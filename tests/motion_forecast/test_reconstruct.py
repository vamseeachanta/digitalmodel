"""Reconstruction core tests (digitalmodel #1358).

Regular-wave analytic checks are the *sole phase guard* (the irregular
cross-check in test_validation is phase-blind), so they verify amplitude AND
signed phase, including the incident-wave spatial-phase transfer term.
"""

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


def test_finite_depth_wavenumber_reduces_to_deep():
    w = 0.9
    assert wavenumber(w, None) == pytest.approx(w * w / GRAVITY)
    # deep-water limit: large depth -> tanh -> 1
    assert wavenumber(w, 100000.0) == pytest.approx(w * w / GRAVITY, rel=1e-6)
    # dispersion holds: omega^2 = g k tanh(k h)
    k = wavenumber(w, 40.0)
    assert w * w == pytest.approx(GRAVITY * k * np.tanh(k * 40.0), rel=1e-9)


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
