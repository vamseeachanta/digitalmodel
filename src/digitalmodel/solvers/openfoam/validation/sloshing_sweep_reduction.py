#!/usr/bin/env python3
"""
ABOUTME: First-harmonic reduction of a forced-roll tank reaction-moment history
into the dm#643 contract row (#641). Fits the moment at the drive frequency by
least squares, which tolerates the non-uniform sampling of an adaptive-timestep
solver, and decomposes it into in-phase and quadrature coefficients.

The sign convention and the exact contract field names are documented on the
sweep harness module, which re-exports this function.
"""

from __future__ import annotations

import math
from typing import Dict, Sequence

import numpy as np

__all__ = ["reduce_roll_moment"]


def reduce_roll_moment(
    times: Sequence[float],
    moment: Sequence[float],
    drive_period: float,
    *,
    fill_level: float,
    roll_amplitude_deg: float,
) -> Dict[str, float]:
    """First-harmonic reduction of a roll-reaction moment history -> contract row.

    Fits ``M(t) ~ M0 + Mc cos(wt) + Ms sin(wt)`` at the drive frequency
    ``w = 2*pi/drive_period`` by least squares (handles the non-uniform sampling
    of an adaptive-timestep solver), then decomposes per the module sign
    convention. Returns exactly the dm#643 contract fields.

    Args:
        times: Sample times (s), need not be uniform.
        moment: Roll-reaction moment about z at each time (N.m).
        drive_period: Imposed roll period (s); ``w = 2*pi/drive_period``.
        fill_level: Fill fraction for this case (carried into the row).
        roll_amplitude_deg: Imposed roll amplitude (deg; carried into the row).

    Returns:
        Dict with the eight contract fields.

    Raises:
        ValueError: If ``drive_period <= 0`` or fewer than 4 matching samples.
    """

    if drive_period <= 0.0:
        raise ValueError(f"drive_period must be > 0, got {drive_period}")
    t = np.asarray(times, dtype=float)
    m = np.asarray(moment, dtype=float)
    if t.size < 4 or t.size != m.size:
        raise ValueError("need >= 4 matching (time, moment) samples")

    omega = 2.0 * math.pi / drive_period
    basis = np.column_stack(
        [np.ones_like(t), np.cos(omega * t), np.sin(omega * t)]
    )
    coef, *_ = np.linalg.lstsq(basis, m, rcond=None)
    _m0, mc, ms = (float(c) for c in coef)

    amplitude = math.hypot(mc, ms)
    # Phase of M relative to theta (theta ~ sin(wt)):
    #   M1 sin(wt + phi) = M1 cos(phi) sin(wt) + M1 sin(phi) cos(wt)
    #   => Ms = M1 cos(phi), Mc = M1 sin(phi) => phi = atan2(Mc, Ms).
    phase = math.atan2(mc, ms)

    return {
        "fill_level": float(fill_level),
        "drive_period": float(drive_period),
        "drive_freq_hz": float(1.0 / drive_period),
        "roll_amplitude_deg": float(roll_amplitude_deg),
        "moment_amplitude": amplitude,
        "moment_phase_rad": phase,
        # component in phase with -theta(t) ~ -sin(wt) -> coefficient of (-sin) is -Ms
        "in_phase_coeff": -ms,
        # component in phase with -theta_dot(t) ~ -cos(wt) -> coefficient of (-cos) is -Mc
        "quad_coeff": -mc,
    }


# ---------------------------------------------------------------------------
# Sweep configuration
# ---------------------------------------------------------------------------
