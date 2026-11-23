# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Dimensional transformations and comparisons for diffraction screening.

Physical relations carry their sources below. Interpolation, comparison
normalisation and resonance bands are screening assumptions rather than
additional measured data.
"""

from __future__ import annotations

import numpy as np
from scipy.optimize import brentq


def normalised_difference(computed: np.ndarray, held: np.ndarray) -> np.ndarray:
    """Return percent differences relative to the held maximum over periods.

    The normalisation is assumed, based on previous experience. Period is
    axis zero. A zero held reference gives NaN, identifying an undefined
    relative comparison rather than introducing an artificial denominator.

    Args:
        computed: Computed values with period on axis zero.
        held: Reference values with identical dimensions.

    Returns:
        Percent differences, or NaN where the held maximum is zero.
    """
    computed = np.asarray(computed, dtype=float)
    held = np.asarray(held, dtype=float)
    if computed.shape != held.shape or held.ndim == 0:
        raise ValueError("Comparison arrays must have matching period dimensions")
    scale = np.max(np.abs(held), axis=0)
    result = np.full(held.shape, np.nan)
    np.divide(100.0 * (computed - held), scale, out=result, where=scale > 0.0)
    return result


def rigid_body_mass_matrix(mass: float, cog: np.ndarray, inertia_at_cog: np.ndarray) -> np.ndarray:
    """Transfer rigid-body inertia from the centre of mass to the body origin.

    Moebs, Ling and Sanny (2016), University Physics Volume 1, section 10.5,
    equation 10.20, gives the parallel-axis theorem. The tensor and cross
    blocks follow from kinetic energy with v_CG = v_origin + omega cross r.
    https://openstax.org/books/university-physics-volume-1/pages/10-5-calculating-moments-of-inertia

    Args:
        mass: Body mass in kg.
        cog: Centre-of-mass offset from the origin, in m.
        inertia_at_cog: Three-by-three inertia tensor in kg.m2.

    Returns:
        Six-by-six mass matrix about the origin.
    """
    r = np.asarray(cog, dtype=float)
    inertia = np.asarray(inertia_at_cog, dtype=float)
    if mass <= 0.0 or not np.isfinite(mass) or r.shape != (3,) or inertia.shape != (3, 3):
        raise ValueError("Mass, centre of mass and inertia tensor have invalid dimensions")
    if not np.all(np.isfinite(r)) or not np.all(np.isfinite(inertia)):
        raise ValueError("Mass properties must be finite")
    x, y, z = r
    cross = np.array([[0.0, -z, y], [z, 0.0, -x], [-y, x, 0.0]])
    transform = np.block([[np.eye(3), -cross], [np.zeros((3, 3)), np.eye(3)]])
    centred = np.zeros((6, 6))
    centred[:3, :3] = mass * np.eye(3)
    centred[3:, 3:] = inertia
    return transform.T @ centred @ transform


def finite_depth_wave_number(period: float, depth: float, gravity: float) -> float:
    """Solve omega squared = g k tanh(k h) for a positive wave number.

    MIT (2005), Marine Hydrodynamics, Lecture 20, section 6.2.4, page 6,
    equation (6), supplies the finite-depth dispersion relation.
    https://ocw.mit.edu/courses/2-20-marine-hydrodynamics-13-021-spring-2005/5d48a5937971d973fd8ca90c051a83f8_lecture20.pdf

    Args:
        period: Wave period in s.
        depth: Water depth in m.
        gravity: Acceleration of gravity in m/s2.

    Returns:
        Wave number in rad/m.
    """
    if not all(np.isfinite(value) and value > 0.0 for value in (period, depth, gravity)):
        raise ValueError("Dispersion inputs must be finite and positive")
    omega_squared = (2.0 * np.pi / period)**2
    upper = omega_squared / gravity
    residual = lambda k: gravity * k * np.tanh(k * depth) - omega_squared
    while residual(upper) < 0.0:
        upper *= 2.0
    return float(brentq(residual, 0.0, upper, xtol=1e-14))


def natural_period(periods: np.ndarray, added_mass: np.ndarray, mass: float, stiffness: float) -> float:
    """Iterate the diagonal natural period on the stored added-mass curve.

    Moebs, Ling and Sanny (2016), University Physics Volume 1, section 15.1,
    equation 15.10, gives T = 2 pi sqrt(m/k). The effective mass is M + A(T).
    Linear interpolation and nearest-endpoint values outside the stored range
    are assumed, based on previous experience; out-of-range results must be
    identified as not bracketed by their caller.
    https://openstax.org/books/university-physics-volume-1/pages/15-1-simple-harmonic-motion

    Args:
        periods: Stored periods in s.
        added_mass: Diagonal added mass at those periods.
        mass: Corresponding rigid-body mass or moment of inertia.
        stiffness: Corresponding restoring coefficient.

    Returns:
        Converged natural period in s.
    """
    periods = np.asarray(periods, dtype=float)
    added_mass = np.asarray(added_mass, dtype=float)
    if periods.ndim != 1 or periods.shape != added_mass.shape or periods.size < 2:
        raise ValueError("Natural-period interpolation needs matching stored curves")
    if mass <= 0.0 or stiffness <= 0.0:
        raise ValueError("Natural-period mass and stiffness must be positive")
    order = np.argsort(periods)
    value = 2.0 * np.pi * np.sqrt(mass / stiffness)
    for _ in range(100):
        effective = mass + np.interp(value, periods[order], added_mass[order])
        if not np.isfinite(effective) or effective <= 0.0:
            raise ValueError("The interpolated effective mass is not positive")
        updated = 2.0 * np.pi * np.sqrt(effective / stiffness)
        if abs(updated - value) <= 1e-6 * updated:
            return float(updated)
        value = updated
    raise ValueError("The stored added-mass natural-period iteration did not converge")


def drag_band_flags(periods: np.ndarray, natural_periods: list[float]) -> np.ndarray:
    """Flag periods within twenty percent of a heave, roll or pitch period.

    The plus/minus twenty-percent band is assumed, based on previous experience.

    Args:
        periods: Wave periods in s.
        natural_periods: Heave, roll and pitch natural periods in s.

    Returns:
        Boolean flags with the same shape as periods.
    """
    periods = np.asarray(periods, dtype=float)
    flagged = np.zeros(periods.shape, dtype=bool)
    for natural in natural_periods:
        flagged |= (periods >= 0.8 * natural) & (periods <= 1.2 * natural)
    return flagged
