#!/usr/bin/env python3
"""
ABOUTME: Derived analyses over the reduced-order sloshing -> vessel-roll
coupling model (dm#643): the time-domain moment feed for a vessel roll equation
of motion, the synthetic single-frequency moment series, anti-roll fill tuning,
and the one-way-coupling escalation check.

These are free functions taking the model rather than methods on it, so the
model module carries the interpolation contract and this module carries what is
computed from it. Each is re-exported as a thin method on the model, which
remains the supported entry point.
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, List, Optional

import numpy as np
from numpy.typing import NDArray


from .sloshing_coupling_models import (
    CouplingStrengthReport,
    FillDampingResult,
    TuningReport,
)

if TYPE_CHECKING:  # pragma: no cover - typing only
    from .sloshing_coupling import SloshingCouplingModel

__all__ = [
    "moment_time_series",
    "moment_from_harmonic",
    "best_antiroll_fill",
    "coupling_strength",
]


def moment_time_series(
    model: "SloshingCouplingModel",
    times: NDArray[np.float64],
    theta: NDArray[np.float64],
    omega: float,
    fill_level: float,
    theta_dot: Optional[NDArray[np.float64]] = None,
) -> NDArray[np.float64]:
    """Sloshing roll-moment time series to add into a vessel roll EOM.

    Given a roll time history ``theta(t)`` (rad) this returns
    ``M_slosh(t) = -in_phase*theta - quad*theta_dot`` sampled at ``times``,
    using the coefficients interpolated at ``(omega, fill_level)``. If
    ``theta_dot`` is not supplied it is estimated with ``numpy.gradient``.

    This is the Phase-A one-way coupling contract: add the returned series to
    the RHS of the vessel roll equation of motion (or feed it as an external
    roll moment to OrcaWave / the time-domain solver).

    Args:
        times: Time array (s), shape (N,).
        theta: Roll angle history (rad), shape (N,).
        omega: Dominant roll circular frequency (rad/s) for coefficient lookup.
        fill_level: Tank fill fraction.
        theta_dot: Optional roll-rate history (rad/s); derived if omitted.

    Returns:
        Sloshing roll moment (N.m), shape (N,).
    """
    times = np.asarray(times, dtype=np.float64)
    theta = np.asarray(theta, dtype=np.float64)
    if times.shape != theta.shape:
        raise ValueError("times and theta must have the same shape")
    if theta_dot is None:
        theta_dot = np.gradient(theta, times)
    else:
        theta_dot = np.asarray(theta_dot, dtype=np.float64)
    c = model.moment_coefficients(omega, fill_level)
    return -c.in_phase_coeff * theta - c.quad_coeff * theta_dot

def moment_from_harmonic(
    model: "SloshingCouplingModel",
    amplitude_deg: float,
    omega: float,
    fill_level: float,
    times: NDArray[np.float64],
) -> NDArray[np.float64]:
    """Moment series for a synthetic single-frequency roll ``A*sin(omega t)``.

    Builds ``theta(t) = A*sin(omega t)`` (A from ``amplitude_deg``) and its
    analytic rate ``theta_dot = A*omega*cos(omega t)`` then returns the
    sloshing moment. Useful for RAO-style checks and the OrcaWave hand-off.
    """
    times = np.asarray(times, dtype=np.float64)
    a = math.radians(amplitude_deg)
    theta = a * np.sin(omega * times)
    theta_dot = a * omega * np.cos(omega * times)
    return model.moment_time_series(
        times, theta, omega, fill_level, theta_dot=theta_dot
    )

# ---- anti-roll tuning ----------------------------------------------- #

def best_antiroll_fill(
    model: "SloshingCouplingModel",
    natural_period_s: Optional[float] = None,
    omega_roll: Optional[float] = None,
) -> TuningReport:
    """Which swept fill best damps roll near the roll natural frequency.

    The core design question for the reverse anti-roll tank: at the vessel
    roll natural frequency, the fill with the largest ``quad_coeff`` (added
    damping) provides the most anti-roll action. Supply either the roll
    natural period (s) or ``omega_roll`` (rad/s).

    Returns:
        A :class:`TuningReport` with per-fill damping and the best fill.
    """
    if omega_roll is None:
        if natural_period_s is None or natural_period_s <= 0.0:
            raise ValueError(
                "Provide natural_period_s > 0 or omega_roll > 0"
            )
        omega_roll = 2.0 * math.pi / natural_period_s
    if natural_period_s is None:
        natural_period_s = 2.0 * math.pi / omega_roll

    per_fill: List[FillDampingResult] = []
    for f in model._fills:
        c = model.moment_coefficients(omega_roll, float(f))
        per_fill.append(
            FillDampingResult(
                fill_level=float(f),
                omega=omega_roll,
                quad_coeff=c.quad_coeff,
                in_phase_coeff=c.in_phase_coeff,
                clamped=c.clamped,
            )
        )
    best = max(per_fill, key=lambda r: r.quad_coeff)
    return TuningReport(
        omega_roll=omega_roll,
        natural_period_s=natural_period_s,
        per_fill=per_fill,
        best_fill=best.fill_level,
        best_quad_coeff=best.quad_coeff,
    )

# ---- one-way-coupling escalation check ------------------------------ #

def coupling_strength(
    model: "SloshingCouplingModel",
    amplitude_deg: float,
    omega: float,
    fill_level: float,
    reference_moment: Optional[float] = None,
    restoring_stiffness: Optional[float] = None,
    threshold: float = 0.15,
) -> CouplingStrengthReport:
    """Ratio of the sloshing moment to a vessel roll reference moment.

    The Phase-A one-way assumption holds only while the sloshing moment is a
    small fraction of the vessel roll balance. This estimates the sloshing
    first-harmonic moment amplitude at ``(amplitude_deg, omega, fill)`` and
    compares it to a reference roll moment. Provide EITHER an explicit
    ``reference_moment`` (N.m, e.g. the wave roll-exciting moment amplitude)
    OR a roll ``restoring_stiffness`` C44 [N.m/rad] -- the reference is then
    ``C44 * A`` (the hydrostatic restoring moment at that roll amplitude).

    Args:
        amplitude_deg: Roll amplitude A (degrees).
        omega: Roll circular frequency (rad/s).
        fill_level: Tank fill fraction.
        reference_moment: Explicit reference roll moment (N.m).
        restoring_stiffness: Roll restoring stiffness C44 [N.m/rad], used to
            build the reference as ``C44 * A`` if ``reference_moment`` is None.
        threshold: Ratio above which two-way iteration is recommended.

    Returns:
        A :class:`CouplingStrengthReport` with the ratio and escalate flag.
    """
    a = math.radians(amplitude_deg)
    c = model.moment_coefficients(omega, fill_level)
    # First-harmonic amplitude of M = -in_phase*A sin - quad*A omega cos.
    m_amp = a * math.hypot(c.in_phase_coeff, c.quad_coeff * omega)

    if reference_moment is None:
        if restoring_stiffness is None or restoring_stiffness <= 0.0:
            raise ValueError(
                "Provide reference_moment or restoring_stiffness > 0"
            )
        reference_moment = restoring_stiffness * a
    if reference_moment <= 0.0:
        raise ValueError("reference_moment must be > 0")

    ratio = m_amp / reference_moment
    return CouplingStrengthReport(
        slosh_moment_amplitude=m_amp,
        reference_moment=reference_moment,
        ratio=ratio,
        threshold=threshold,
        escalate=ratio > threshold,
    )
