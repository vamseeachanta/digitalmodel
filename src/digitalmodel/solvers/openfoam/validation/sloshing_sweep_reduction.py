# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
#!/usr/bin/env python3
"""
ABOUTME: First-harmonic reduction of a forced-roll tank reaction-moment history
into the dm#643 contract row (#641). Fits the moment at the drive frequency by
least squares, which tolerates the non-uniform sampling of an adaptive-timestep
solver, and decomposes it into in-phase and quadrature coefficients.

The sign convention and the exact contract field names are documented on the
sweep harness module, which re-exports this function.

The separate normalized interface requires a declared settled window and returns
physical stiffness/damping with fit diagnostics; raw contract rows are retained.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Dict, Sequence

import numpy as np

__all__ = ["NormalizedRollReduction", "reduce_roll_moment", "reduce_roll_moment_normalized"]


@dataclass(frozen=True)
class NormalizedRollReduction:
    """Normalized coefficients and sample-weighted harmonic-fit diagnostics.

    ``a`` and ``b`` are signed cosine and sine moment amplitudes, respectively.
    ``residual_rms`` belongs to the joint DC + harmonics 1, 2, 3 fit;
    ``first_harmonic_residual_rms`` belongs to a separate DC + harmonic 1 fit.
    ``harmonic_ratio`` is sqrt(A2**2 + A3**2) / A1, undefined (None) for
    exactly zero A1. No near-zero cutoff or residual acceptance limit is used.
    ``selected_window`` and ``cycles`` describe actual retained sample coverage.
    """

    a: float
    b: float
    mean_moment: float
    stiffness: float
    damping: float
    theta0_rad: float
    omega_rad_s: float
    residual_rms: float
    first_harmonic_residual_rms: float
    harmonic_amplitudes: Dict[int, float]
    harmonic_ratio: float | None
    condition_number: float
    selected_window: tuple[float, float]
    cycles: float
    sample_count: int

    @property
    def units(self) -> Dict[str, str]:
        """Return explicit units for physical coefficients and diagnostics."""
        return {
            "a": "N.m", "b": "N.m", "mean_moment": "N.m",
            "stiffness": "N.m/rad", "damping": "N.m.s/rad",
            "theta0_rad": "rad", "omega_rad_s": "rad/s",
            "residual_rms": "N.m", "first_harmonic_residual_rms": "N.m",
            "harmonic_amplitudes": "N.m", "harmonic_ratio": "1",
            "condition_number": "1", "selected_window": "s", "cycles": "1",
            "sample_count": "1",
        }


def reduce_roll_moment_normalized(
    times: Sequence[float],
    moment: Sequence[float],
    drive_period: float,
    *,
    roll_amplitude_deg: float,
    settled_window: tuple[float, float],
    min_cycles: float,
    max_condition_number: float,
) -> NormalizedRollReduction:
    """Fit settled reaction moments and normalize for declared sine motion.

    The declared motion is theta(t) = theta0*sin(omega*t), with absolute
    sample times referenced to that same phase origin. Differentiation gives
    theta_dot = omega*theta0*cos(omega*t). Hence M = -K*theta-B*theta_dot
    implies a = -B*omega*theta0 and b = -K*theta0, so B = -a/(omega*theta0)
    and K = -b/theta0. Degrees convert by theta0 = degrees*pi/180.
    These identities are the mathematical basis; no empirical constants enter.
    K is an effective dynamic stiffness at the drive frequency; the fit cannot
    separate inertia from elastic stiffness at a single frequency.

    The caller must select an inclusive settled_window in seconds, wholly
    within the supplied record. No automatic settling assessment is made.
    min_cycles is a caller-selected minimum sampled span / drive_period,
    at least one complete cycle. max_condition_number is a finite limit >= 1
    on the spectral condition number of the unscaled joint design matrix.
    Neither has a default engineering acceptance value.

    Fit DC and cosine/sine pairs for harmonics 1, 2, 3 jointly by unweighted
    least squares. Second and third harmonics diagnose departures from a
    sinusoid; they are not an exhaustive spectrum or a quality verdict.
    Uneven sampling is supported. Every retained gap must be strictly less
    than drive_period/(2*3), the half-period of the highest fitted harmonic:
    the Nyquist spacing bound, with equality excluded for phase ambiguity.
    This gap rule plus full numerical rank and the caller's conditioning
    limit reject ambiguous fits; arbitrary higher-frequency aliasing cannot
    be excluded from sampled data alone. More samples than the seven fitted
    parameters are required for residual degrees of freedom.

    Args:
        times: Strictly increasing finite sample times in seconds.
        moment: Matching finite roll reaction moments in N.m.
        drive_period: Positive finite drive period in seconds.
        roll_amplitude_deg: Positive finite imposed displacement amplitude.
        settled_window: Required inclusive start and end times in seconds.
        min_cycles: Explicit minimum number of complete sampled drive cycles.
        max_condition_number: Explicit maximum design-matrix condition number.

    Returns:
        Signed raw terms, physical coefficients, units and fit diagnostics.

    Raises:
        ValueError: For invalid motion, grid, window, policies or singular fit.
    """
    omega, theta0, velocity_amplitude = _normalization_motion(
        drive_period, roll_amplitude_deg, min_cycles, max_condition_number,
    )
    t, m, cycles = _settled_roll_samples(
        times, moment, settled_window, drive_period, min_cycles,
    )
    return _fit_normalized_roll(
        t, m, drive_period, theta0, omega, velocity_amplitude,
        max_condition_number, cycles,
    )


def _normalization_motion(
    drive_period, roll_amplitude_deg, min_cycles, max_condition_number,
):
    """Validate motion and caller policies before converting motion units."""
    for name, value in (
        ("drive_period", drive_period), ("roll_amplitude_deg", roll_amplitude_deg),
        ("min_cycles", min_cycles), ("max_condition_number", max_condition_number),
    ):
        if not math.isfinite(value) or value <= 0.0:
            raise ValueError(f"{name} must be positive and finite")
    if min_cycles < 1.0 or max_condition_number < 1.0:
        raise ValueError("min_cycles and max_condition_number must be >= 1")
    omega = 2.0 * math.pi / drive_period
    theta0 = math.radians(roll_amplitude_deg)
    velocity_amplitude = omega * theta0
    if not all(math.isfinite(v) and v > 0.0 for v in (omega, theta0, velocity_amplitude)):
        raise ValueError("motion normalization must be positive and finite")
    return omega, theta0, velocity_amplitude


def _settled_roll_samples(times, moment, settled_window, drive_period, min_cycles):
    """Validate the complete record and retain a resolved settled window."""
    t = np.asarray(times, dtype=float)
    m = np.asarray(moment, dtype=float)
    if t.ndim != 1 or m.ndim != 1 or t.size != m.size or t.size == 0:
        raise ValueError("need matching nonempty one-dimensional samples")
    if not np.all(np.isfinite(t)) or not np.all(np.isfinite(m)):
        raise ValueError("all samples must be finite, including outside the window")
    if np.any(t[1:] <= t[:-1]):
        raise ValueError("sample times must be strictly increasing without duplicates")
    window = np.asarray(settled_window, dtype=float)
    if window.shape != (2,) or not np.all(np.isfinite(window)):
        raise ValueError("settled_window must contain two finite bounds")
    start, end = window
    if start >= end or start < t[0] or end > t[-1]:
        raise ValueError("settled_window must be ordered and within the record")
    keep = (t >= start) & (t <= end)
    t, m = t[keep], m[keep]
    # DC + three cosine/sine pairs gives seven fitted parameters.
    if t.size <= 7:
        raise ValueError("settled_window needs more than seven samples")
    cycles = float((t[-1] - t[0]) / drive_period)
    if not math.isfinite(cycles) or cycles < min_cycles:
        raise ValueError("settled_window contains insufficient sampled cycles")
    if np.any(np.diff(t) >= drive_period / (2.0 * 3.0)):
        raise ValueError("sampling gaps must resolve the third harmonic")
    return t, m, cycles


def _roll_harmonic_basis(t, drive_period):
    """Build the joint DC and three-harmonic design matrix."""
    # Reduce absolute time modulo the period before forming bounded phases.
    phase = (np.remainder(t, drive_period) / drive_period) * (2.0 * math.pi)
    columns = [np.ones_like(t)]
    for order in (1, 2, 3):
        columns.extend((np.cos(order * phase), np.sin(order * phase)))
    basis = np.column_stack(columns)
    return basis


def _fit_normalized_roll(
    t, m, drive_period, theta0, omega, velocity_amplitude,
    max_condition_number, cycles,
):
    """Fit retained moments and assemble physical coefficients and diagnostics."""
    basis = _roll_harmonic_basis(t, drive_period)
    # Scale moments to keep fitting/residual arithmetic away from overflow.
    scale = float(np.max(np.abs(m))) or 1.0
    scaled_m = m / scale
    coef, _, rank, singular = np.linalg.lstsq(basis, scaled_m, rcond=None)
    if rank != basis.shape[1]:
        raise ValueError("sampling gives a rank-deficient harmonic fit")
    condition = float(singular[0] / singular[-1])
    if not math.isfinite(condition) or condition > max_condition_number:
        raise ValueError("sampling exceeds max_condition_number")
    first_coef, *_ = np.linalg.lstsq(basis[:, :3], scaled_m, rcond=None)
    rms = float(np.sqrt(np.mean((scaled_m - basis @ coef) ** 2)) * scale)
    first_rms = float(
        np.sqrt(np.mean((scaled_m - basis[:, :3] @ first_coef) ** 2)) * scale
    )
    mean, a, b = (float(c * scale) for c in coef[:3])
    amplitudes = {
        order: math.hypot(float(coef[2 * order - 1]), float(coef[2 * order])) * scale
        for order in (1, 2, 3)
    }
    ratio = (math.hypot(amplitudes[2], amplitudes[3]) / amplitudes[1]
             if amplitudes[1] > 0.0 else None)
    stiffness, damping = -b / theta0, -a / velocity_amplitude
    values = [mean, a, b, stiffness, damping, rms, first_rms, *amplitudes.values()]
    if ratio is not None:
        values.append(ratio)
    if not all(math.isfinite(v) for v in values):
        raise ValueError("fitted coefficients or diagnostics are not finite")
    return NormalizedRollReduction(
        a=a, b=b, mean_moment=mean, stiffness=stiffness, damping=damping,
        theta0_rad=theta0, omega_rad_s=omega, residual_rms=rms,
        first_harmonic_residual_rms=first_rms, harmonic_amplitudes=amplitudes,
        harmonic_ratio=ratio, condition_number=condition,
        selected_window=(float(t[0]), float(t[-1])), cycles=cycles,
        sample_count=int(t.size),
    )


def reduce_roll_moment(
    times: Sequence[float],
    moment: Sequence[float],
    drive_period: float,
    *,
    fill_level: float,
    roll_amplitude_deg: float,
) -> Dict[str, float]:
    """First-harmonic reduction of a roll-reaction moment history -> contract row.

    This retained raw interface is not normalized: in_phase_coeff and
    quad_coeff remain moment amplitudes in N.m and fit every supplied sample.
    Use reduce_roll_moment_normalized for physical K/B and a settled window.

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
