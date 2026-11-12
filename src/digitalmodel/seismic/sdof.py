# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Pure linear SDOF response to prescribed ground acceleration, in SI units.

The equation is m*u'' + c*u' + k*u = -m*ag, where u is relative to the
moving base, w = 2*pi/T, k = m*w² and c = 2*zeta*m*w. Dividing by m gives
u'' + 2*zeta*w*u' + w²*u = -ag, so mass cancels at fixed T and zeta.

Algorithm reference: OpenSees documentation, "Newmark Method", theory and
average-acceleration equations:
https://opensees.github.io/OpenSeesDocumentation/user/manual/analysis/integrator/Newmark.html

Only the implicit average-acceleration method is supported. Trapezoidal
integration gives v[n+1] = v[n] + h*(a[n]+a[n+1])/2. Taking the average
endpoint velocity for displacement gives u[n+1] = u[n] + h*v[n] +
h²*(a[n]+a[n+1])/4. Thus gamma = 1/2 and beta = gamma² = 1/4, derived
from the two trapezoidal averages rather than configurable constants.
These are the second-order, unconditionally stable linear Newmark parameters;
stability does not guarantee accuracy for a coarse time step.

Write u_pred = u[n]+h*v[n]+(1/2-beta)*h²*a[n] and
v_pred = v[n]+(1-gamma)*h*a[n]. Substitution into equilibrium yields
a[n+1] = (-ag[n+1]-2*zeta*w*v_pred-w²*u_pred) /
         (1+2*zeta*w*gamma*h+w²*beta*h²).
Then u[n+1] = u_pred+beta*h²*a[n+1], v[n+1] = v_pred+gamma*h*a[n+1].
The initial acceleration is obtained from the same equilibrium equation.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np


@dataclass(frozen=True)
class SDOFResult:
    """SI response samples at the unchanged input time origin.

    Displacement (m), velocity (m/s) and relative acceleration (m/s²) refer
    to motion relative to the base. Absolute acceleration (m/s²) is relative
    acceleration plus the supplied ground acceleration. Arrays are newly
    allocated; the frozen container does not make their contents immutable.
    """

    time_s: np.ndarray
    relative_displacement_m: np.ndarray
    relative_velocity_m_s: np.ndarray
    relative_acceleration_m_s2: np.ndarray
    absolute_acceleration_m_s2: np.ndarray


def integrate_sdof(
    time_s: list[float] | np.ndarray,
    ground_acceleration_m_s2: list[float] | np.ndarray,
    *,
    natural_period_s: float,
    damping_ratio: float = 0.0,
    mass_kg: float = 1.0,
    initial_displacement_m: float = 0.0,
    initial_velocity_m_s: float = 0.0,
    method: str = "average_acceleration",
) -> SDOFResult:
    """Integrate a linear base-driven oscillator without I/O or preprocessing.

    Args:
        time_s: Finite, increasing, uniformly spaced seconds, at least two.
            Uniformity allows a relative interval difference of 1e-8 and no
            absolute tolerance floor, including for very short time steps.
            This is an input validation tolerance, not an accuracy bound.
        ground_acceleration_m_s2: Finite base acceleration in m/s², with the
            same one-dimensional shape as time_s; no unit conversion occurs.
        natural_period_s: Positive finite undamped natural period in seconds.
        damping_ratio: Finite nonnegative fraction of critical damping;
            zero (the default), critical and overdamped cases are supported.
        mass_kg: Positive finite mass in kilograms; the default unit mass
            is a normalization since mass cancels from the base-driven ODE.
        initial_displacement_m: Finite relative displacement at time_s[0].
        initial_velocity_m_s: Finite relative velocity at time_s[0].
        method: Only "average_acceleration" is supported. Integration
            parameters are fixed by the derivation in the module docstring.

    Returns:
        Relative displacement, velocity and acceleration, and absolute
        acceleration at every input time, including the equilibrium initial
        acceleration. The first time may be nonzero or negative; it is never
        reset. Neither input array is modified or shared with the result.

    Raises:
        ValueError: Invalid samples, physical inputs, unsupported method or
            arithmetic outside the representable floating-point range.
    """
    if method != "average_acceleration":
        raise ValueError("method must be average_acceleration")
    time = np.array(time_s, dtype=float, copy=True)
    ground = np.array(ground_acceleration_m_s2, dtype=float, copy=True)
    if time.ndim != 1 or ground.ndim != 1:
        raise ValueError("time_s and ground_acceleration_m_s2 must be one-dimensional")
    if time.size != ground.size:
        raise ValueError(
            "time_s and ground_acceleration_m_s2 must have the same length"
        )
    if time.size < 2:
        raise ValueError("time_s must contain at least two samples")
    if not np.all(np.isfinite(time)) or not np.all(np.isfinite(ground)):
        raise ValueError("time_s and ground_acceleration_m_s2 must be finite")
    with np.errstate(over="ignore"):
        steps = np.diff(time)
    if not np.all(np.isfinite(steps)):
        raise ValueError("time_s intervals must be finite")
    if np.any(steps <= 0.0):
        raise ValueError("time_s must be strictly increasing")
    # Policy: interval deviations greater than one part in 100 million fail.
    if not np.allclose(steps, steps[0], rtol=1e-8, atol=0.0):
        raise ValueError("time_s must use a uniform time step")

    period = _finite_scalar("natural_period_s", natural_period_s)
    mass = _finite_scalar("mass_kg", mass_kg)
    damping = _finite_scalar("damping_ratio", damping_ratio)
    u0 = _finite_scalar("initial_displacement_m", initial_displacement_m)
    v0 = _finite_scalar("initial_velocity_m_s", initial_velocity_m_s)
    if period <= 0.0:
        raise ValueError("natural_period_s must be positive")
    if mass <= 0.0:
        raise ValueError("mass_kg must be positive")
    if damping < 0.0:
        raise ValueError("damping_ratio must be nonnegative")

    u = np.empty_like(time)
    v = np.empty_like(time)
    a = np.empty_like(time)
    u[0], v[0] = u0, v0
    h = steps[0]
    gamma = 1.0 / 2.0  # Endpoint averaging, derived above.
    beta = gamma * gamma  # Two endpoint averages, derived above.
    try:
        with np.errstate(over="raise", invalid="raise", divide="raise"):
            w = 2.0 * np.pi / period
            stiffness_per_mass = w * w
            damping_per_mass = 2.0 * damping * w
            a[0] = -ground[0] - damping_per_mass * v0 - stiffness_per_mass * u0
            displacement_weight = beta * h * h
            velocity_weight = gamma * h
            denominator = (
                1.0 + damping_per_mass * velocity_weight
                + stiffness_per_mass * displacement_weight
            )
            for i in range(1, time.size):
                u_pred = u[i - 1] + h * v[i - 1] + (gamma - beta) * h * h * a[i - 1]
                v_pred = v[i - 1] + (1.0 - gamma) * h * a[i - 1]
                a[i] = (
                    -ground[i] - damping_per_mass * v_pred
                    - stiffness_per_mass * u_pred
                ) / denominator
                u[i] = u_pred + displacement_weight * a[i]
                v[i] = v_pred + velocity_weight * a[i]
            absolute = a + ground
    except (FloatingPointError, OverflowError) as exc:
        raise ValueError(
            "inputs exceed the floating-point range for SDOF response"
        ) from exc
    return SDOFResult(time, u, v, a, absolute)


def _finite_scalar(name: str, value: float) -> np.float64:
    """Reject nonfinite or nonscalar physical parameters with a named error."""
    if np.ndim(value) != 0:
        raise ValueError(f"{name} must be a finite scalar")
    try:
        scalar = np.float64(value)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError(f"{name} must be a finite scalar") from exc
    if not np.isfinite(scalar):
        raise ValueError(f"{name} must be a finite scalar")
    return scalar
