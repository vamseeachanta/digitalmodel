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

The standalone ``coupled_roll_response`` instead requires explicit normalized
coefficients and solves the linear forced roll equation without a sweep model.
"""

from __future__ import annotations

import math
from numbers import Complex, Real
from typing import TYPE_CHECKING, List, Optional

import numpy as np
from numpy.typing import NDArray


from .sloshing_coupling_models import (
    CoupledRollResponse,
    CouplingStrengthReport,
    FillDampingResult,
    NormalizedRollCoefficients,
    TuningReport,
)

if TYPE_CHECKING:  # pragma: no cover - typing only
    from .sloshing_coupling import SloshingCouplingModel

__all__ = [
    "moment_time_series",
    "moment_from_harmonic",
    "best_antiroll_fill",
    "coupling_strength",
    "coupled_roll_response",
]


def _finite_real(value: float, name: str) -> float:
    """Require a finite real scalar, excluding boolean flags."""
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite real scalar")
    value = float(value)
    if not math.isfinite(value):
        raise ValueError(f"{name} must be finite")
    return value


def _support_interval(bounds: tuple[float, float], name: str) -> tuple[float, float]:
    """Validate an immutable inclusive support interval."""
    if not isinstance(bounds, tuple) or len(bounds) != 2:
        raise ValueError(f"{name} must be a pair of finite bounds")
    low, high = (_finite_real(value, name) for value in bounds)
    if low < 0.0 or low > high:
        raise ValueError(f"{name} must have ordered nonnegative bounds")
    return low, high


def _roll_impedance(inertia: float, restoring: float, damping: float,
                    omega: float, name: str) -> complex:
    """Form the harmonic impedance and reject an undefined steady solution."""
    real = _finite_real(restoring - inertia * omega * omega, f"{name} impedance")
    imag = _finite_real(omega * damping, f"{name} impedance")
    impedance = complex(real, imag)
    if impedance == 0.0:
        raise ValueError(f"{name}: singular undamped resonance")
    return impedance


def _validated_roll_coefficients(
    coefficients: NormalizedRollCoefficients, omega: float,
) -> tuple[float, float, float, float]:
    """Validate coefficient provenance and support independently of the solver."""
    if not isinstance(coefficients, NormalizedRollCoefficients):
        raise TypeError("coefficients must be NormalizedRollCoefficients")
    tank = coefficients
    if tank.normalization != "per_roll_angle_and_rate":
        raise ValueError("explicit per_roll_angle_and_rate normalization required")
    if tank.stiffness_unit != "N.m/rad" or tank.damping_unit != "N.m/(rad/s)":
        raise ValueError("declared units must be N.m/rad and N.m/(rad/s)")
    if not isinstance(tank.condition, str) or not tank.condition.strip():
        raise ValueError("coefficient condition provenance must be declared")

    stiffness = _finite_real(tank.stiffness, "tank stiffness")
    tank_damping = _finite_real(tank.damping, "tank damping")
    evaluated_omega = _finite_real(tank.omega_rad_s, "evaluated frequency")
    reference_amplitude = _finite_real(tank.reference_amplitude_rad, "reference amplitude")
    if min(evaluated_omega, reference_amplitude) <= 0.0:
        raise ValueError("frequencies and reference amplitude must be positive")
    freq_low, freq_high = _support_interval(tank.frequency_support_rad_s, "frequency support")
    amp_low, amp_high = _support_interval(tank.amplitude_support_rad, "amplitude support")
    if freq_low <= 0.0:
        raise ValueError("frequency support bounds must be positive")
    if not freq_low <= omega <= freq_high:
        raise ValueError("frequency outside declared coefficient support")
    if evaluated_omega != omega:
        raise ValueError("coefficient evaluated frequency must match requested frequency")
    if not amp_low <= reference_amplitude <= amp_high:
        raise ValueError("reference amplitude outside declared coefficient support")

    return stiffness, tank_damping, amp_low, amp_high


def _scaled_roll_response(excitation: complex, impedance: complex,
                          name: str) -> tuple[complex, float]:
    """Divide bounded mantissas, then restore the power-of-two scale."""
    # Independent binary scales keep both operands bounded even when their
    # magnitudes span the float range. F/Z = (Fm/Zm)*2**(Fe-Ze).
    force_exponent = math.frexp(max(abs(excitation.real), abs(excitation.imag)))[1]
    impedance_exponent = math.frexp(max(abs(impedance.real), abs(impedance.imag)))[1]
    force = complex(math.ldexp(excitation.real, -force_exponent),
                    math.ldexp(excitation.imag, -force_exponent))
    normalized = complex(math.ldexp(impedance.real, -impedance_exponent),
                         math.ldexp(impedance.imag, -impedance_exponent))
    quotient = force / normalized
    exponent = force_exponent - impedance_exponent
    try:
        response = complex(math.ldexp(quotient.real, exponent),
                           math.ldexp(quotient.imag, exponent))
    except OverflowError as error:
        raise ValueError(f"{name} response must be finite") from error
    # hypot avoids overflow from squaring the real and imaginary components.
    amplitude = _finite_real(math.hypot(response.real, response.imag), f"{name} response amplitude")
    if amplitude == 0.0:
        raise ValueError("response amplitudes must be representable and nonzero")
    return response, amplitude


def coupled_roll_response(
    *,
    inertia_kg_m2: float,
    restoring_nm_per_rad: float,
    damping_nm_s_per_rad: float,
    omega_rad_s: float,
    excitation_moment_nm: complex,
    coefficients: NormalizedRollCoefficients,
) -> CoupledRollResponse:
    """Solve a linear single-degree-of-freedom forced roll equation.

    The sign and phase convention is ``theta(t) = Re(Theta*exp(i*omega*t))``
    and ``M_tank = -K*theta - B_tank*theta_dot``. Substitution into
    ``I*theta_ddot + B*theta_dot + C*theta = M_excitation + M_tank`` gives
    ``Z0 = C - I*omega**2 + i*omega*B`` and
    ``Zc = C + K - I*omega**2 + i*omega*(B + B_tank)``. Each complex response
    is ``F/Z`` in radians for the same complex excitation ``F`` in N.m.
    Inertia is in kg.m^2, restoring in N.m/rad, damping in N.m.s/rad and
    frequency in rad/s. These equations derive all numeric factors here.

    Reduction is ``100*(1 - abs(Theta_c)/abs(Theta_0))`` percent. Negative
    reduction is retained: reactive stiffness can move the oscillator closer
    to resonance even with nonnegative tank damping. No damping benefit is
    inferred from a damping coefficient alone.

    Only explicitly normalized coefficients with declared units are accepted.
    The evaluated frequency must match the requested frequency exactly and
    lie inside declared support. The reference amplitude and computed coupled
    amplitude must lie inside declared amplitude support. There is no clamping,
    extrapolation, amplitude iteration or physical calibration. The caller must
    justify applicability across the declared support and encode the tank
    configuration, fill and coefficient source in ``condition``.

    All inputs and computed results must be finite. Inertia, baseline and total
    restoring, frequency and reference amplitude must be positive; both damping
    contributions must be nonnegative. Nonzero excitation is required to define
    a reduction percentage. Exact undamped resonance in either solution is
    rejected; damped resonance is supported. Unrepresentable responses are
    rejected rather than returned as zero or infinity. Complex division uses
    binary scaling; impedance construction must still remain finite.
    """
    inertia = _finite_real(inertia_kg_m2, "inertia")
    restoring = _finite_real(restoring_nm_per_rad, "restoring")
    damping = _finite_real(damping_nm_s_per_rad, "baseline damping")
    omega = _finite_real(omega_rad_s, "frequency")
    if min(inertia, restoring, omega) <= 0.0:
        raise ValueError("inertia, restoring, frequencies and reference amplitude must be positive")
    stiffness, tank_damping, amp_low, amp_high = _validated_roll_coefficients(coefficients, omega)
    if damping < 0.0 or tank_damping < 0.0:
        raise ValueError("baseline and tank damping must be nonnegative")
    total_restoring = _finite_real(restoring + stiffness, "total restoring")
    total_damping = _finite_real(damping + tank_damping, "total damping")
    if total_restoring <= 0.0:
        raise ValueError("total restoring must be positive")

    if isinstance(excitation_moment_nm, bool) or not isinstance(excitation_moment_nm, Complex):
        raise ValueError("excitation moment must be a finite complex scalar")
    excitation = complex(excitation_moment_nm)
    _finite_real(excitation.real, "excitation moment real part")
    _finite_real(excitation.imag, "excitation moment imaginary part")
    if excitation == 0.0:
        raise ValueError("excitation moment must be nonzero")

    baseline, baseline_amplitude = _scaled_roll_response(
        excitation, _roll_impedance(inertia, restoring, damping, omega, "baseline"), "baseline")
    coupled, coupled_amplitude = _scaled_roll_response(
        excitation, _roll_impedance(inertia, total_restoring, total_damping, omega, "coupled"), "coupled")
    if not amp_low <= coupled_amplitude <= amp_high:
        raise ValueError("computed coupled amplitude outside declared coefficient support")
    reduction = _finite_real(100.0 * (1.0 - coupled_amplitude / baseline_amplitude), "reduction")

    return CoupledRollResponse(
        baseline_response_rad=baseline,
        coupled_response_rad=coupled,
        baseline_amplitude_rad=baseline_amplitude,
        coupled_amplitude_rad=coupled_amplitude,
        reduction_percent=reduction,
        inertia_kg_m2=inertia,
        restoring_nm_per_rad=restoring,
        damping_nm_s_per_rad=damping,
        omega_rad_s=omega,
        excitation_moment_nm=excitation,
        coefficients=coefficients,
    )


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
