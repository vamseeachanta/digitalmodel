# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Linear response and narrow-band spectral statistics.

JONSWAP shape follows the existing wave-spectrum implementation. Normalisation
is integrated on the positive frequency axis, not truncated to the RAO grid.
Rayleigh maxima use N=T/(2*pi*sqrt(m0/m2)), following Longuet-Higgins (1952).
"""
from __future__ import annotations

import math

import numpy as np
from scipy.integrate import quad
from scipy.linalg import eig
from scipy.optimize import brentq, linear_sum_assignment

from .basis import aggregate

_trapezoid = getattr(np, "trapezoid", None) or np.trapz


def spectrum(omega: np.ndarray, hs: float, tp: float, gamma: float) -> np.ndarray:
    """Return a normalised JONSWAP spectrum in m² per rad/s."""
    if hs < 0 or tp <= 0 or gamma < 1 or np.any(omega <= 0):
        raise ValueError("Positive frequencies/period and gamma >= 1 are required")
    peak = 2*math.pi/tp

    def shape(x: float) -> float:
        if x <= 0:
            return 0.0
        sigma = 0.07 if x <= 1 else 0.09
        value: float = x**-5 * math.exp(-1.25/x**4) * gamma**math.exp(-0.5*((x-1)/sigma)**2)
        return value
    norm = quad(shape, 0, 1, epsabs=1e-10)[0]+quad(shape, 1, np.inf, epsabs=1e-10)[0]
    return hs**2/16/peak/norm*np.array([shape(float(x)) for x in omega/peak])


def wave_statistics(omega: np.ndarray, rao: np.ndarray, density: np.ndarray,
                     duration: float) -> dict:
    """Return RMS, significant single amplitude and most probable maximum."""
    energy = abs(rao)**2*density
    m0 = float(_trapezoid(energy, omega))
    m2 = float(_trapezoid(omega**2*energy, omega))
    if m0 == 0 or m2 == 0:
        return {"rms": 0.0, "significant": 0.0, "maximum": 0.0, "m0": m0, "m2": m2}
    cycles = duration/(2*math.pi*math.sqrt(m0/m2))
    if cycles <= 1:
        raise ValueError("Rayleigh maximum requires more than one response cycle")
    return {"rms": math.sqrt(m0), "significant": 2*math.sqrt(m0),
            "maximum": math.sqrt(2*m0*math.log(cycles)), "m0": m0, "m2": m2}


def mass_matrix(basis: dict, reading: str, symmetric: bool = True) -> np.ndarray:
    """Construct SWL rigid mass matrix; optionally impose reflection symmetry."""
    data = aggregate(basis, reading)
    cg = data["cg"].copy()
    inertia = data["inertia_swl"].copy()
    if symmetric:
        cg[1] = 0.0
        inertia[0, 1] = inertia[1, 0] = 0.0
        inertia[1, 2] = inertia[2, 1] = 0.0
    x, y, z = cg
    skew = np.array([[0, -z, y], [z, 0, -x], [-y, x, 0]])
    matrix = np.zeros((6, 6))
    matrix[:3, :3] = data["mass"]*np.eye(3)
    matrix[:3, 3:] = -data["mass"]*skew
    matrix[3:, :3] = data["mass"]*skew
    matrix[3:, 3:] = inertia
    return matrix


def natural_periods(omega: np.ndarray, added: np.ndarray, mass: np.ndarray,
                    stiffness: np.ndarray) -> np.ndarray:
    """Solve det[K-ω²(M+A(ω))]=0, retaining six-degree rigid-body coupling.

    Eigenbranches are ordered by eigenvalue. Dominant mass-weighted modal
    participation assigns the six roots to displacement/rotation labels.
    """
    def modes(frequency: float) -> tuple:
        a = np.array([[np.interp(frequency, omega, added[:, i, j])
                       for j in range(6)] for i in range(6)])
        values, vectors = eig(stiffness, mass+a)
        if np.max(abs(values.imag)) > 1e-6 * max(abs(values.real)):
            raise ValueError("Complex undamped natural-frequency branch")
        order = np.argsort(values.real)
        return values.real[order], vectors[:, order], mass+a

    roots, participation = [], []
    for branch in range(6):
        def characteristic(frequency: float) -> float:
            value: float = modes(frequency)[0][branch]-frequency**2
            return value
        changes = [(left, right) for left, right in zip(omega[:-1], omega[1:])
                   if characteristic(left)*characteristic(right) < 0]
        if not changes:
            raise ValueError("Natural-frequency root lies outside computed BEM range")
        root = brentq(characteristic, *changes[0])
        roots.append(root)
        _, vectors, effective = modes(root)
        weights = abs(vectors[:, branch])**2*abs(np.diag(effective))
        participation.append(weights/weights.sum())
    branches, axes = linear_sum_assignment(-np.array(participation))
    periods = np.zeros(6)
    for branch, axis in zip(branches, axes):
        periods[axis] = 2*math.pi/roots[branch]
    return periods


def response_operators(omega: np.ndarray, added: np.ndarray, radiation: np.ndarray,
                       excitation: np.ndarray, mass: np.ndarray,
                       stiffness: np.ndarray, damping: float) -> np.ndarray:
    """Solve [K-ω²(M+A)-iω(B+Bextra)]q=F using the BEM phasor convention.

    Source: Capytaine 3.0.0, post_pro.impedance.rao_transfer_function.
    """
    result = np.zeros_like(excitation, dtype=complex)
    for index, frequency in enumerate(omega):
        critical = 2*np.sqrt(np.maximum(0, np.diag(mass+added[index])*np.diag(stiffness)))
        extra = np.zeros(6)
        extra[[0, 2, 4]] = damping*critical[[0, 2, 4]]
        impedance = (stiffness-frequency**2*(mass+added[index])
                     - 1j*frequency*(radiation[index]+np.diag(extra)))
        result[index] = np.linalg.solve(impedance, excitation[index].T).T
    return result
