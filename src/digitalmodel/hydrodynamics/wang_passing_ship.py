"""Wang closed-form passing-ship forces on a moored vessel (legacy port).

Port of a legacy in-house passing-ship force calculator (Excel/VBA
``modPassingShip`` + ``modQuadrature``, 2014-2016 era, cross-checked
against a companion MathCAD sheet) implementing S. Wang's slender-body
method:

    S. Wang, "Dynamic Effects of Ship Passage on Moored Vessels",
    Journal of the Waterways, Harbors and Coastal Engineering Division,
    ASCE, Vol. 101, WW3, p. 247, 1975.

The same formulation (with the finite-depth method of images) is given in
Varyani, Krishnankutty & Vantorre, "Prediction of Load on Mooring Ropes of
a Container Ship due to the Forces Induced by a Passing Bulk Carrier",
MARSIM'03, Eqs. (2.4)-(2.9).

Method
======

Both hulls are represented by parabolic sectional-area curves

    S_i(x) = A_i * (1 - 4 x^2 / L_i^2),   S_i'(x) = -8 A_i x / L_i^2

where ``L_i`` is the length and ``A_i`` the midship sectional area.  With
stagger ``xi`` (midship-to-midship, positive when the passing ship is
ahead of the moored ship) and lateral separation ``eta`` (centreline to
centreline), the deep-water surge/sway forces and yaw moment on the
moored ship are

    X(xi, eta) = rho U^2 / (2 pi) * I[ S1'(x1) * F(x1) ]
    Y(xi, eta) = rho U^2 eta / pi * I[ S1'(x1) * G(x1) ]
    N(xi, eta) = rho U^2 eta / pi * I[ (S1'(x1) x1 + S1(x1)) * G(x1) ]

with ``I[.]`` the integral over the moored-ship length and the inner
(passing-ship) integrals evaluated in closed form.  For the parabolic
area curve the legacy code integrates analytically, with
``R = x2 - x1 + xi``:

    F(x1) = -8 A2/L2^2 * [ ln(sqrt(eta^2+R^2) + R)
                           - x2 / sqrt(eta^2+R^2) ]_{x2=-L2/2}^{+L2/2}
    G(x1) = -8 A2/L2^2 * [ ((x1 - xi) R - eta^2)
                           / (eta^2 sqrt(eta^2+R^2)) ]_{x2=-L2/2}^{+L2/2}

(The bracketed expressions are the antiderivatives of
``S2'(x2) R / (eta^2+R^2)^{3/2}`` and ``S2'(x2) / (eta^2+R^2)^{3/2}``
respectively, i.e. Wang's kernels.)

**Finite depth** (needed when depth < ~2x draft) uses the method of
images: with ``eta_n = sqrt(eta^2 + 4 n^2 h^2)`` and image count
``n = -N..N`` (legacy N = 10, i.e. 21 terms):

    X_h = sum_n X(xi, eta_n)
    Y_h = eta * sum_n Y(xi, eta_n) / eta_n
    N_h = eta * sum_n N(xi, eta_n) / eta_n

Sign conventions (legacy): positive sway = moored ship attracted toward
the passing ship (peak attraction near zero stagger); surge and yaw are
odd functions of stagger for symmetric hulls.

Units: any consistent set.  The legacy tool used ft, ft^2, slug/ft^3 and
ft/s, giving lbf and ft-lbf; SI inputs give N and N-m.

Numerical scheme
================

* Production path (:func:`wang_passing_forces` and the per-component
  functions): the outer moored-ship integral is evaluated with fixed-order
  Gauss-Legendre quadrature, fully vectorised over stagger with NumPy.
* Reference path (:func:`wang_forces_scalar`): a faithful transliteration
  of the legacy VBA adaptive quadrature — 5-point Gauss-Lobatto with a
  6-point error estimate and interval bisection to a relative tolerance of
  1e-6 — in pure Python floats.  (The loose legacy source skips the first
  of its five initial panels due to an off-by-one; the validated
  spreadsheet/MathCAD outputs correspond to the full-domain integral,
  which both paths here compute.)
* The log term of the surge antiderivative is evaluated in the
  cancellation-free form ``ln(eta^2 / (sqrt(eta^2+R^2) - R))`` for
  ``R < 0``.

Validated against the legacy MathCAD worksheet (finite-depth stagger
sweeps and spot values, agreement ~1e-9 relative) and a legacy workbook
output table (201-point stagger sweep at infinite and finite depth,
agreement within the 4-significant-figure rounding of the stored values).
See ``tests/hydrodynamics/test_wang_passing_ship.py``.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from functools import lru_cache
from typing import Callable, Optional, Sequence, Union

import numpy as np

__all__ = [
    "DEFAULT_N_IMAGES",
    "DEFAULT_QUAD_NODES",
    "WangVessel",
    "WangForceResult",
    "wang_surge",
    "wang_sway",
    "wang_yaw",
    "wang_passing_forces",
    "wang_forces_scalar",
]

# Number of bottom images on each side of n = 0 used for the finite-depth
# correction (legacy VBA and MathCAD both sum n = -10..10, 21 terms).
DEFAULT_N_IMAGES = 10

# Gauss-Legendre nodes for the outer (moored-ship) integral of the
# production path.  96 nodes resolves the legacy validation cases to
# better than 1e-9 relative; the integrand is smooth for eta > 0.
DEFAULT_QUAD_NODES = 96

ArrayLike = Union[float, Sequence[float], np.ndarray]


@dataclass(frozen=True)
class WangVessel:
    """Parabolic-sectional-area vessel: length and midship area.

    ``midship_area`` is the immersed midship cross-section area,
    typically ``beam * draft * midship_coefficient``.
    """

    length: float
    midship_area: float

    def __post_init__(self) -> None:
        if not self.length > 0.0:
            raise ValueError("WangVessel.length must be positive")
        if not self.midship_area > 0.0:
            raise ValueError("WangVessel.midship_area must be positive")


@dataclass(frozen=True)
class WangForceResult:
    """Surge/sway forces and yaw moment for each stagger value."""

    stagger: np.ndarray
    surge: np.ndarray
    sway: np.ndarray
    yaw: np.ndarray


# ----------------------------------------------------------------------
# Closed-form inner (passing-ship) integrals
# ----------------------------------------------------------------------

def _f_antiderivative(x1, xi, eta, x2):
    """Antiderivative (in x2) of R / (eta^2 + R^2)^(3/2) * x2-weighting.

    Legacy ``funcFInt``: ln(sqrt(eta^2+R^2) + R) - x2/sqrt(eta^2+R^2),
    with R = x2 - x1 + xi.  The log is evaluated in a cancellation-free
    form for R < 0.
    """
    R = x2 - x1 + xi
    root = np.sqrt(eta * eta + R * R)
    log_term = np.where(
        R >= 0.0,
        np.log(root + R),
        np.log((eta * eta) / (root - R)),
    )
    return log_term - x2 / root


def _g_antiderivative(x1, xi, eta, x2):
    """Antiderivative (in x2) of 1 / (eta^2 + R^2)^(3/2) weighted by x2.

    Legacy ``funcGInt``: ((x1 - xi) R - eta^2) / (eta^2 sqrt(eta^2+R^2)),
    with R = xi + x2 - x1.
    """
    R = xi + x2 - x1
    root = np.sqrt(eta * eta + R * R)
    return ((x1 - xi) * R - eta * eta) / (eta * eta * root)


def _func_f(x1, xi, eta, passing: WangVessel):
    """Legacy ``funcF``: inner integral for the surge kernel."""
    half = passing.length / 2.0
    scale = -8.0 * passing.midship_area / passing.length**2
    return scale * (
        _f_antiderivative(x1, xi, eta, half)
        - _f_antiderivative(x1, xi, eta, -half)
    )


def _func_g(x1, xi, eta, passing: WangVessel):
    """Legacy ``funcG``: inner integral for the sway/yaw kernel."""
    half = passing.length / 2.0
    scale = -8.0 * passing.midship_area / passing.length**2
    return scale * (
        _g_antiderivative(x1, xi, eta, half)
        - _g_antiderivative(x1, xi, eta, -half)
    )


# ----------------------------------------------------------------------
# Production path: vectorised Gauss-Legendre over the moored ship
# ----------------------------------------------------------------------

@lru_cache(maxsize=8)
def _leggauss(n_nodes: int):
    nodes, weights = np.polynomial.legendre.leggauss(n_nodes)
    return nodes, weights


def _moored_nodes(moored: WangVessel, n_nodes: int):
    """Gauss-Legendre nodes/weights mapped to [-L1/2, L1/2]."""
    nodes, weights = _leggauss(n_nodes)
    half = moored.length / 2.0
    return nodes * half, weights * half


def _outer_integral(
    kernel: Callable[[np.ndarray, np.ndarray, float], np.ndarray],
    xi: np.ndarray,
    eta: float,
    moored: WangVessel,
    n_nodes: int,
) -> np.ndarray:
    """Integrate ``kernel(x1, xi, eta)`` over the moored-ship length.

    ``x1`` enters as column (quadrature nodes), ``xi`` as row, so the
    kernel evaluates on a (n_nodes, n_stagger) grid in one shot.
    """
    x1, w = _moored_nodes(moored, n_nodes)
    values = kernel(x1[:, None], xi[None, :], eta)
    return w @ values


def _validate_scalars(separation: float, velocity: float, density: float) -> None:
    if not separation > 0.0:
        raise ValueError("separation must be positive (centreline to centreline)")
    if not density > 0.0:
        raise ValueError("density must be positive")
    if not np.isfinite(velocity):
        raise ValueError("velocity must be finite")


def _as_array(stagger: ArrayLike):
    xi = np.atleast_1d(np.asarray(stagger, dtype=float))
    if xi.ndim != 1:
        raise ValueError("stagger must be a scalar or 1-D array")
    return xi


def _surge_deep(xi, eta, moored, passing, velocity, density, n_nodes):
    a1, l1 = moored.midship_area, moored.length

    def kernel(x1, xi_row, eta_val):
        ds1 = -8.0 * a1 * x1 / l1**2
        return ds1 * _func_f(x1, xi_row, eta_val, passing)

    integral = _outer_integral(kernel, xi, eta, moored, n_nodes)
    return integral * density * velocity**2 / (2.0 * math.pi)


def _sway_deep(xi, eta, moored, passing, velocity, density, n_nodes):
    a1, l1 = moored.midship_area, moored.length

    def kernel(x1, xi_row, eta_val):
        ds1 = -8.0 * a1 * x1 / l1**2
        return ds1 * _func_g(x1, xi_row, eta_val, passing)

    integral = _outer_integral(kernel, xi, eta, moored, n_nodes)
    return integral * density * velocity**2 * eta / math.pi


def _yaw_deep(xi, eta, moored, passing, velocity, density, n_nodes):
    a1, l1 = moored.midship_area, moored.length

    def kernel(x1, xi_row, eta_val):
        ds1 = -8.0 * a1 * x1 / l1**2
        s1 = (1.0 - 4.0 * x1**2 / l1**2) * a1
        return (ds1 * x1 + s1) * _func_g(x1, xi_row, eta_val, passing)

    integral = _outer_integral(kernel, xi, eta, moored, n_nodes)
    return integral * density * velocity**2 * eta / math.pi


def _image_etas(separation: float, depth: float, n_images: int):
    """(eta_n, n) pairs for the method of images, n = -N..N."""
    if not depth > 0.0:
        raise ValueError("depth must be positive (or None for deep water)")
    return [
        math.sqrt(separation**2 + 4.0 * n**2 * depth**2)
        for n in range(-n_images, n_images + 1)
    ]


def _depth_sum(
    deep_func,
    xi,
    separation,
    moored,
    passing,
    velocity,
    density,
    depth,
    n_images,
    n_nodes,
    lateral_weighting: bool,
):
    """Image sum. ``lateral_weighting`` applies eta/eta_n (sway, yaw)."""
    total = np.zeros_like(xi)
    for eta_n in _image_etas(separation, depth, n_images):
        term = deep_func(xi, eta_n, moored, passing, velocity, density, n_nodes)
        if lateral_weighting:
            term = term * (separation / eta_n)
        total += term
    return total


def _component(
    deep_func,
    lateral_weighting: bool,
    stagger: ArrayLike,
    separation: float,
    moored: WangVessel,
    passing: WangVessel,
    velocity: float,
    density: float,
    depth: Optional[float],
    n_images: int,
    n_nodes: int,
):
    _validate_scalars(separation, velocity, density)
    xi = _as_array(stagger)
    if depth is None:
        out = deep_func(xi, separation, moored, passing, velocity, density, n_nodes)
    else:
        out = _depth_sum(
            deep_func, xi, separation, moored, passing, velocity, density,
            depth, n_images, n_nodes, lateral_weighting,
        )
    if np.isscalar(stagger) or np.asarray(stagger).ndim == 0:
        return float(out[0])
    return out


def wang_surge(
    stagger: ArrayLike,
    separation: float,
    moored: WangVessel,
    passing: WangVessel,
    velocity: float,
    density: float,
    depth: Optional[float] = None,
    n_images: int = DEFAULT_N_IMAGES,
    n_nodes: int = DEFAULT_QUAD_NODES,
):
    """Surge force on the moored ship (deep water, or finite ``depth``)."""
    return _component(
        _surge_deep, False, stagger, separation, moored, passing,
        velocity, density, depth, n_images, n_nodes,
    )


def wang_sway(
    stagger: ArrayLike,
    separation: float,
    moored: WangVessel,
    passing: WangVessel,
    velocity: float,
    density: float,
    depth: Optional[float] = None,
    n_images: int = DEFAULT_N_IMAGES,
    n_nodes: int = DEFAULT_QUAD_NODES,
):
    """Sway force on the moored ship (positive = toward passing ship)."""
    return _component(
        _sway_deep, True, stagger, separation, moored, passing,
        velocity, density, depth, n_images, n_nodes,
    )


def wang_yaw(
    stagger: ArrayLike,
    separation: float,
    moored: WangVessel,
    passing: WangVessel,
    velocity: float,
    density: float,
    depth: Optional[float] = None,
    n_images: int = DEFAULT_N_IMAGES,
    n_nodes: int = DEFAULT_QUAD_NODES,
):
    """Yaw moment on the moored ship about its midship point."""
    return _component(
        _yaw_deep, True, stagger, separation, moored, passing,
        velocity, density, depth, n_images, n_nodes,
    )


def wang_passing_forces(
    stagger: ArrayLike,
    separation: float,
    moored: WangVessel,
    passing: WangVessel,
    velocity: float,
    density: float,
    depth: Optional[float] = None,
    n_images: int = DEFAULT_N_IMAGES,
    n_nodes: int = DEFAULT_QUAD_NODES,
) -> WangForceResult:
    """All three components over a stagger sweep (vectorised)."""
    xi = _as_array(stagger)
    args = (separation, moored, passing, velocity, density, depth, n_images, n_nodes)
    return WangForceResult(
        stagger=xi,
        surge=np.asarray(wang_surge(xi, *args)),
        sway=np.asarray(wang_sway(xi, *args)),
        yaw=np.asarray(wang_yaw(xi, *args)),
    )


# ----------------------------------------------------------------------
# Reference path: legacy adaptive Gauss-Lobatto quadrature (pure Python)
# ----------------------------------------------------------------------

# 5-point Gauss-Lobatto: exact for polynomials up to degree 7.
_GL5_POINTS = (-1.0, -math.sqrt(3.0 / 7.0), 0.0, math.sqrt(3.0 / 7.0), 1.0)
_GL5_WEIGHTS = (0.1, 49.0 / 90.0, 32.0 / 45.0, 49.0 / 90.0, 0.1)

# 6-point Gauss-Lobatto: used for the error estimate of each panel.
_GL6_POINTS = (
    -1.0,
    -math.sqrt(1.0 / 3.0 + 2.0 * math.sqrt(7.0) / 21.0),
    -math.sqrt(1.0 / 3.0 - 2.0 * math.sqrt(7.0) / 21.0),
    math.sqrt(1.0 / 3.0 - 2.0 * math.sqrt(7.0) / 21.0),
    math.sqrt(1.0 / 3.0 + 2.0 * math.sqrt(7.0) / 21.0),
    1.0,
)
_GL6_WEIGHTS = (
    1.0 / 15.0,
    (14.0 - math.sqrt(7.0)) / 30.0,
    (14.0 + math.sqrt(7.0)) / 30.0,
    (14.0 + math.sqrt(7.0)) / 30.0,
    (14.0 - math.sqrt(7.0)) / 30.0,
    1.0 / 15.0,
)


def _lobatto(f, a, b, points, weights):
    c = 0.5 * (a + b)
    h = 0.5 * (b - a)
    return h * sum(w * f(c + h * p) for p, w in zip(points, weights))


def _adaptive_lobatto(f, a, b, eps, whole, max_recursion=50):
    """Legacy ``AdaptiveIntegrate``: bisect until GL5 halves match GL6 whole."""
    if max_recursion <= 0:
        raise RuntimeError("adaptive quadrature exceeded max recursion depth")
    c = 0.5 * (a + b)
    left = _lobatto(f, a, c, _GL5_POINTS, _GL5_WEIGHTS)
    right = _lobatto(f, c, b, _GL5_POINTS, _GL5_WEIGHTS)
    if abs(left + right - whole) <= eps:
        return left + right
    return (
        _adaptive_lobatto(f, a, c, eps / 2.0, left, max_recursion - 1)
        + _adaptive_lobatto(f, c, b, eps / 2.0, right, max_recursion - 1)
    )


def _integrate_legacy(f, lower, upper, n_panels=5, rel_err=1e-6, min_precision=1e-12):
    """Legacy ``Integrate`` over the *full* domain.

    The loose legacy source's panel loop skipped its first panel
    (off-by-one); the validated tool outputs correspond to the full
    domain, integrated here.
    """
    dx = (upper - lower) / n_panels
    total = 0.0
    for i in range(n_panels):
        a = lower + i * dx
        b = a + dx
        whole = _lobatto(f, a, b, _GL6_POINTS, _GL6_WEIGHTS)
        if abs(whole) > min_precision:
            total += _adaptive_lobatto(f, a, b, abs(rel_err * whole), whole)
    return total


def _scalar_deep(xi, eta, moored, passing, velocity, density):
    """(surge, sway, yaw) at one stagger/separation, deep water, scalar path."""
    a1, l1 = moored.midship_area, moored.length

    def ds1(x1):
        return -8.0 * a1 * x1 / l1**2

    def s1(x1):
        return (1.0 - 4.0 * x1**2 / l1**2) * a1

    def f_func(x1):
        return float(_func_f(x1, xi, eta, passing))

    def g_func(x1):
        return float(_func_g(x1, xi, eta, passing))

    half = l1 / 2.0
    surge = _integrate_legacy(lambda x: ds1(x) * f_func(x), -half, half)
    sway = _integrate_legacy(lambda x: ds1(x) * g_func(x), -half, half)
    yaw = _integrate_legacy(lambda x: (ds1(x) * x + s1(x)) * g_func(x), -half, half)
    scale = density * velocity**2
    return (
        surge * scale / (2.0 * math.pi),
        sway * scale * eta / math.pi,
        yaw * scale * eta / math.pi,
    )


def wang_forces_scalar(
    stagger: float,
    separation: float,
    moored: WangVessel,
    passing: WangVessel,
    velocity: float,
    density: float,
    depth: Optional[float] = None,
    n_images: int = DEFAULT_N_IMAGES,
):
    """Scalar reference path: legacy adaptive Gauss-Lobatto quadrature.

    Returns ``(surge, sway, yaw)`` floats.  Slower than
    :func:`wang_passing_forces`; retained as the validation oracle for
    the vectorised path (see tests).
    """
    _validate_scalars(separation, velocity, density)
    xi = float(stagger)
    if depth is None:
        return _scalar_deep(xi, separation, moored, passing, velocity, density)
    surge = sway = yaw = 0.0
    for eta_n in _image_etas(separation, depth, n_images):
        s, w, n = _scalar_deep(xi, eta_n, moored, passing, velocity, density)
        surge += s
        sway += w * (separation / eta_n)
        yaw += n * (separation / eta_n)
    return surge, sway, yaw
