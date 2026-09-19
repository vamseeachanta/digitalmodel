"""ABOUTME: Semi-analytical heave radiation of a truncated vertical cylinder, as an independent reference for panel solvers.

A boundary-element result checked only against another boundary-element result
is not validated, and this repository holds no third-party reference data: the
files under docs/domains/orcawave/L00_validation_wamit carry no WAMIT output,
and every benchmark report there compares OrcaWave against itself.

This module supplies a reference computed by different mathematics. The fluid
is divided at the cylinder radius into the region beneath the body and the
region outside it, each expanded in its own set of eigenfunctions, and the two
expansions are matched across the shared cylindrical interface. Nothing about
the method resembles a panel discretisation, so agreement between the two is
evidence about both.

Geometry and conventions
------------------------
Circular cylinder of radius ``a`` and draft ``d``, axis vertical, floating in
water of depth ``h``. The free surface is z = 0, the seabed z = -h, and the gap
beneath the cylinder has height H = h - d. The potential is written
Phi = Re(phi * exp(-i*omega*t)) for unit heave velocity amplitude, so on the
cylinder base d(phi)/dz = 1 and on its side wall d(phi)/dr = 0.

Accuracy
--------
The base, seabed and free-surface conditions are satisfied to around 1e-14.
Potential continuity across the interface converges as the mode count rises
(5.0e-2 at 20 modes to 3.6e-3 at 160). The radial velocity carries an
inverse-square-root singularity at the sharp edge where the base meets the
wall, so the velocity jump at the interface converges only logarithmically;
this is a property of the geometry, not of the truncation, and it does not
affect the force, which integrates the potential over the smooth base.

Against OrcaWave 11.6c on a uniformly refined mesh of the same cylinder,
Richardson extrapolation of the damping agrees with this solution to 0.04% at
omega = 1.0 rad/s and 0.13% at omega = 2.0 rad/s, at observed orders 1.80 and
1.48. The added mass converges monotonically from above, reaching 1.0% at 3456
panels and still falling.

Reference: the matched-eigenfunction treatment of the truncated vertical
cylinder given by Yeung, "Added mass and damping of a vertical cylinder in
finite-depth waters", Applied Ocean Research 3(3), 1981.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

import numpy as np
from scipy.optimize import brentq
from scipy.special import hankel1, iv, kv

__all__ = [
    "CylinderGeometry",
    "HeaveCoefficients",
    "heave_added_mass_damping",
    "wavenumbers",
]

STANDARD_GRAVITY = 9.80665


@dataclass(frozen=True)
class CylinderGeometry:
    """A truncated vertical circular cylinder floating at the free surface."""

    radius: float
    draft: float
    water_depth: float

    def __post_init__(self) -> None:
        if self.radius <= 0:
            raise ValueError("radius must be positive")
        if self.draft <= 0:
            raise ValueError("draft must be positive")
        if self.draft >= self.water_depth:
            raise ValueError(
                f"draft {self.draft} must be less than water depth "
                f"{self.water_depth}; the cylinder would rest on the seabed"
            )

    @property
    def gap(self) -> float:
        """Height of water beneath the cylinder."""
        return self.water_depth - self.draft

    @property
    def displaced_volume(self) -> float:
        return math.pi * self.radius ** 2 * self.draft


@dataclass(frozen=True)
class HeaveCoefficients:
    """Heave radiation coefficients at one frequency, in SI units."""

    omega: float
    added_mass: float
    damping: float
    modes: int
    residual: float

    @property
    def period(self) -> float:
        return 2.0 * math.pi / self.omega


def wavenumbers(
    omega: float, water_depth: float, count: int, gravity: float = STANDARD_GRAVITY
) -> tuple[float, np.ndarray]:
    """Propagating and evanescent wavenumbers for the free-surface problem.

    The propagating root solves omega^2 = g k tanh(k h); the evanescent roots
    solve omega^2 = -g k tan(k h), one in each interval
    ((m - 1/2) pi, m pi) / h.
    """
    if omega <= 0:
        raise ValueError("omega must be positive")
    if count < 0:
        raise ValueError("count must not be negative")

    target = omega ** 2 * water_depth / gravity

    def propagating(x: float) -> float:
        return x * math.tanh(x) - target

    upper = max(2.0, 2.0 * target + 2.0)
    while propagating(upper) < 0.0:
        upper *= 2.0
    k0 = brentq(propagating, 1e-12, upper, xtol=1e-14, rtol=8.9e-16) / water_depth

    roots = []
    for m in range(1, count + 1):
        lo = (m - 0.5) * math.pi
        hi = m * math.pi
        eps = 1e-10

        def evanescent(x: float) -> float:
            return -x * math.tan(x) - target

        roots.append(
            brentq(evanescent, lo + eps, hi - eps, xtol=1e-14, rtol=8.9e-16)
            / water_depth
        )
    return k0, np.asarray(roots)


def _depth_normalisations(
    k0: float, k: np.ndarray, h: float
) -> tuple[float, np.ndarray]:
    """Scale factors making the vertical eigenfunctions orthonormal over the depth."""
    n0 = math.sqrt(0.5 * (1.0 + math.sinh(2 * k0 * h) / (2 * k0 * h)))
    nm = np.sqrt(0.5 * (1.0 + np.sin(2 * k * h) / (2 * k * h)))
    return n0, nm


def _int_cos_cosh(lam: float, k: float, height: float) -> float:
    """Integral of cos(lam u) cosh(k u) over u in [0, height]."""
    return (
        k * math.sinh(k * height) * math.cos(lam * height)
        + lam * math.cosh(k * height) * math.sin(lam * height)
    ) / (k ** 2 + lam ** 2)


def _int_cos_cos(lam: float, k: float, height: float) -> float:
    """Integral of cos(lam u) cos(k u) over u in [0, height]."""
    if abs(lam - k) < 1e-12:
        if k <= 0:
            return height
        return 0.5 * (height + math.sin(2 * k * height) / (2 * k))
    return 0.5 * (
        math.sin((k - lam) * height) / (k - lam)
        + math.sin((k + lam) * height) / (k + lam)
    )


def heave_added_mass_damping(
    omega: float,
    geometry: CylinderGeometry,
    water_density: float = 1025.0,
    gravity: float = STANDARD_GRAVITY,
    modes: int = 60,
) -> HeaveCoefficients:
    """Heave added mass and radiation damping at a single frequency.

    ``modes`` truncates both eigenfunction series. The damping is converged to
    better than 0.1% by roughly 40 modes; the added mass converges more slowly
    because of the edge singularity and drifts by about 0.25% between 20 and
    120 modes.
    """
    if modes < 1:
        raise ValueError("modes must be at least 1")

    a, d, h = geometry.radius, geometry.draft, geometry.water_depth
    gap = geometry.gap
    order = modes

    k0, k_evanescent = wavenumbers(omega, h, order, gravity)
    n0, nm = _depth_normalisations(k0, k_evanescent, h)
    lam = np.array([0.0] + [n * math.pi / gap for n in range(1, order + 1)])

    # Overlap of the interior vertical modes with the exterior ones, taken
    # across the gap only, since that is where the two regions touch.
    overlap = np.zeros((order + 1, order + 1))
    for n in range(order + 1):
        overlap[0, n] = _int_cos_cosh(lam[n], k0, gap) / n0
        for m in range(1, order + 1):
            overlap[m, n] = _int_cos_cos(lam[n], k_evanescent[m - 1], gap) / nm[m - 1]

    # Logarithmic radial derivatives at the interface, for radial functions
    # normalised to unity there.
    radial_slope = np.empty(order + 1, dtype=complex)
    radial_slope[0] = -k0 * hankel1(1, k0 * a) / hankel1(0, k0 * a)
    for m in range(1, order + 1):
        km = k_evanescent[m - 1]
        radial_slope[m] = -km * kv(1, km * a) / kv(0, km * a)

    interior_slope = np.zeros(order + 1)
    for n in range(1, order + 1):
        x = lam[n] * a
        interior_slope[n] = iv(1, x) / iv(0, x)

    size = 2 * (order + 1)
    matrix = np.zeros((size, size), dtype=complex)
    rhs = np.zeros(size, dtype=complex)

    # Radial velocity is continuous across the gap and vanishes on the wall,
    # projected onto the exterior modes over the full depth.
    for m in range(order + 1):
        matrix[m, (order + 1) + m] = radial_slope[m] * h
        for n in range(1, order + 1):
            matrix[m, n] = -lam[n] * interior_slope[n] * overlap[m, n]
        rhs[m] = -(a / (2 * gap)) * overlap[m, 0]

    # The potential is continuous across the gap, projected onto the interior
    # modes over the gap.
    for n in range(order + 1):
        row = (order + 1) + n
        for m in range(order + 1):
            matrix[row, (order + 1) + m] = overlap[m, n]
        matrix[row, n] = -(gap if n == 0 else gap / 2.0)
        rhs[row] = (
            gap ** 2 / 6.0 - a ** 2 / 4.0
            if n == 0
            else gap ** 2 * ((-1) ** n) / (n ** 2 * math.pi ** 2)
        )

    solution = np.linalg.solve(matrix, rhs)
    residual = float(
        np.linalg.norm(matrix @ solution - rhs) / max(np.linalg.norm(rhs), 1e-300)
    )
    interior = solution[: order + 1]

    # Vertical force from the pressure over the cylinder base; the side wall
    # has no vertical normal component and contributes nothing.
    integral = (1.0 / (2 * gap)) * (gap ** 2 * a ** 2 / 2.0 - a ** 4 / 8.0)
    integral = integral + interior[0] * a ** 2 / 2.0
    for n in range(1, order + 1):
        x = lam[n] * a
        integral = integral + interior[n] * ((-1) ** n) * (a / lam[n]) * iv(1, x) / iv(0, x)
    integral = 2.0 * math.pi * integral

    return HeaveCoefficients(
        omega=omega,
        added_mass=float(water_density * integral.real),
        damping=float(omega * water_density * integral.imag),
        modes=order,
        residual=residual,
    )
