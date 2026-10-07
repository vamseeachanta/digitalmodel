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

Accuracy, and what has and has not been established
---------------------------------------------------
The base, seabed and free-surface conditions are satisfied to around 1e-14,
and none of them is imposed pointwise, so they are evidence rather than
tautology. Potential continuity across the interface converges as the mode
count rises, from 5.0e-2 at 20 modes to 3.6e-3 at 160.

The fluid occupies a wedge of interior angle 3*pi/2 at the edge where the base
meets the side wall, so the leading velocity behaviour there goes as
distance**(-1/3). The radial-velocity jump at the interface is therefore
dominated by the neighbourhood of that edge and converges slowly in the mode
count. Integrability of that singularity does not by itself establish that the
integrated force is immune to truncation, and no such immunity is claimed here;
what is measured is that the damping moves by less than 0.1% and the added mass
by less than 0.5% between 40 and 120 modes for the geometries under test.

Against OrcaWave 11.6c on a uniformly refined mesh of the same cylinder
(96, 384 and 1536 panels), Richardson extrapolation of the damping lands within
0.04% of this solution at omega = 1.0 rad/s and 0.13% at omega = 2.0 rad/s, at
observed orders 1.80 and 1.48. Three levels fix the limit, the coefficient and
the order together without independently testing the asymptotic model, so that
agreement is provisional proximity rather than an established error bound. The
same procedure returns a meaningless order for the added mass, which is why no
extrapolated added-mass figure is quoted; the added mass is reported only as
converging monotonically from above, 3.39% to 0.99% at 3456 panels.

Reference: the matched-eigenfunction treatment of the truncated vertical
cylinder given by Yeung, "Added mass and damping of a vertical cylinder in
finite-depth waters", Applied Ocean Research 3(3), 1981.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

import numpy as np
from scipy.optimize import brentq
from scipy.special import hankel1, ive, kve

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
    """Scale factors making the vertical eigenfunctions orthonormal over the depth.

    The propagating factor is returned already divided by exp(k0*h). Deep water
    or a high frequency drives k0*h into the hundreds, where sinh overflows, and
    every place this factor is used divides by it in company with a cosh that
    grows at the same rate, so carrying the scaling through cancels exactly.
    """
    x = 2.0 * k0 * h
    # sinh(x)/x * exp(-x) = (1 - exp(-2x)) / (2x), finite for every positive x.
    scaled = 0.5 * (math.exp(-x) + (1.0 - math.exp(-2.0 * x)) / (2.0 * x))
    n0_scaled = math.sqrt(scaled)
    nm = np.sqrt(0.5 * (1.0 + np.sin(2 * k * h) / (2 * k * h)))
    return n0_scaled, nm


def _int_cos_cosh_scaled(lam: float, k: float, height: float, h: float) -> float:
    """Integral of cos(lam u) cosh(k u) over [0, height], divided by exp(k*h).

    Written with non-positive exponents only, since height never exceeds the
    water depth, so nothing overflows however deep the water is.
    """
    a = math.exp(k * (height - h))
    b = math.exp(-k * (height + h))
    sinh_scaled = 0.5 * (a - b)
    cosh_scaled = 0.5 * (a + b)
    return (k * sinh_scaled * math.cos(lam * height)
            + lam * cosh_scaled * math.sin(lam * height)) / (k ** 2 + lam ** 2)


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


def _solve(
    omega: float,
    geometry: CylinderGeometry,
    gravity: float,
    modes: int,
) -> dict:
    """Assemble and solve the matching system, returning everything it produced.

    Exposed privately so that tests can reconstruct the potential from the
    coefficients this function actually returns, rather than from a parallel
    reimplementation that could agree with a wrong answer.
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
        # Both the integral and the normalisation carry exp(k0*h); dividing one
        # scaled quantity by the other cancels it exactly.
        overlap[0, n] = _int_cos_cosh_scaled(lam[n], k0, gap, h) / n0
        for m in range(1, order + 1):
            overlap[m, n] = _int_cos_cos(lam[n], k_evanescent[m - 1], gap) / nm[m - 1]

    # Logarithmic radial derivatives at the interface, for radial functions
    # normalised to unity there. The modified Bessel functions are taken in
    # their exponentially scaled forms, whose scaling cancels in each ratio;
    # taken unscaled, a shallow gap drives the argument past the point where
    # iv and kv overflow and the ratio degenerates to inf/inf.
    radial_slope = np.empty(order + 1, dtype=complex)
    radial_slope[0] = -k0 * hankel1(1, k0 * a) / hankel1(0, k0 * a)
    for m in range(1, order + 1):
        km = k_evanescent[m - 1]
        radial_slope[m] = -km * kve(1, km * a) / kve(0, km * a)

    interior_slope = np.zeros(order + 1)
    for n in range(1, order + 1):
        x = lam[n] * a
        interior_slope[n] = ive(1, x) / ive(0, x)

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
    return dict(
        interior=solution[: order + 1],
        exterior=solution[order + 1:],
        lam=lam, k0=k0, k_evanescent=k_evanescent,
        n0_scaled=n0, nm=nm, order=order, gap=gap,
        radius=a, draft=d, water_depth=h, residual=residual,
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
    s = _solve(omega, geometry, gravity, modes)
    a, d, h = geometry.radius, geometry.draft, geometry.water_depth
    gap, order, lam = s["gap"], s["order"], s["lam"]
    interior, residual = s["interior"], s["residual"]

    # Vertical force from the pressure over the cylinder base; the side wall
    # has no vertical normal component and contributes nothing.
    integral = (1.0 / (2 * gap)) * (gap ** 2 * a ** 2 / 2.0 - a ** 4 / 8.0)
    integral = integral + interior[0] * a ** 2 / 2.0
    for n in range(1, order + 1):
        x = lam[n] * a
        integral = (integral + interior[n] * ((-1) ** n) * (a / lam[n])
                    * ive(1, x) / ive(0, x))
    integral = 2.0 * math.pi * integral

    added_mass = float(water_density * integral.real)
    damping = float(omega * water_density * integral.imag)
    if not (math.isfinite(added_mass) and math.isfinite(damping)
            and math.isfinite(residual)):
        raise ArithmeticError(
            f"non-finite result at omega={omega}, radius={a}, draft={d}, "
            f"depth={h}, modes={order}; the mode count may be too high for "
            f"this geometry"
        )

    return HeaveCoefficients(
        omega=omega,
        added_mass=added_mass,
        damping=damping,
        modes=order,
        residual=residual,
    )
