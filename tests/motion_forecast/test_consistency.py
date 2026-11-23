"""Derived-quantity consistency vs closed form (digitalmodel #1359).

A single-component sea through an analytic RAO gives an exact heave
``a|H|cos(ωt+ψ)``, so the derived excursion/velocity magnitudes have known peaks
(``a|H|`` and ``a|H|ω``). Deterministic — no sampling scatter — and it exercises
reconstruct -> vertical_motion_at -> time_derivative end to end.
"""

import numpy as np

from digitalmodel.motion_forecast import (
    AnalyticRAO,
    WaveComponent,
    WaveForecast,
    reconstruct_motion,
)
from digitalmodel.motion_forecast.derived import (
    heave_velocity_magnitude,
    vertical_excursion_magnitude,
)


def test_lever_arm_excursion_end_to_end():
    """A pitch-only sea + non-zero offset exercises vertical_motion_at's lever
    arm through reconstruct: z_tip = -rx*deg2rad(pitch), peak rx*deg2rad(a*|Hp|)."""
    w0, a0, magp, rx = 0.6, 1.0, 2.0, 30.0  # Hp in deg/m
    fc = WaveForecast([WaveComponent(w0, a0, 0.0, heading=0.0)], horizon=60.0)
    rao = AnalyticRAO({"pitch": lambda w, b: magp + 0j})
    m = reconstruct_motion(fc, rao, dt=0.02)
    exc = vertical_excursion_magnitude(m, (rx, 0.0, 0.0))
    assert abs(exc.max() - rx * np.deg2rad(a0 * magp)) < 1e-4


def test_single_component_derived_peaks_match_closed_form():
    w0, a0, phi0 = 0.7, 1.4, 0.3
    mag, ang = 0.8, 0.5
    fc = WaveForecast([WaveComponent(w0, a0, phi0, heading=0.0)], horizon=60.0)
    rao = AnalyticRAO({"heave": lambda w, b: mag * np.exp(1j * ang)})
    m = reconstruct_motion(fc, rao, dt=0.02)

    # excursion at origin = |heave|; peak = a0*mag (exact)
    exc = vertical_excursion_magnitude(m, (0.0, 0.0, 0.0))
    assert abs(exc.max() - a0 * mag) < 1e-4

    # velocity magnitude peak = a0*mag*w0 (numerical derivative)
    vel = heave_velocity_magnitude(m)
    assert abs(vel.max() - a0 * mag * w0) / (a0 * mag * w0) < 5e-3
