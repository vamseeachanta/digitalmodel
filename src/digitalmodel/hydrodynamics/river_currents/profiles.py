#!/usr/bin/env python3
"""
ABOUTME: River / shallow-water vertical current-velocity profile models
(power-law and logarithmic boundary-layer) for subsea/riser and barge
current-loading assessments.

Unlike the open-ocean DNV-RP-C205 power law (which references the *surface*
speed and decays with depth-below-surface), river profiles are governed by the
bed boundary layer: the velocity is zero (or small) at the river bed and grows
toward the surface. Both conventions are supported here through an explicit
``height_above_bed`` coordinate.

References
----------
- Open-channel power law: Chow, V.T. (1959), "Open-Channel Hydraulics";
  Chen, C.L. (1991), "Unified theory on power laws for flow resistance",
  J. Hydraul. Eng. 117(3). u(z') = u_surface * (z'/H)**(1/m).
- Logarithmic law of the wall: Prandtl–von Karman; Julien, P.Y. (2002),
  "River Mechanics", Cambridge Univ. Press, Ch. 6.
  u(z') = (u_star / kappa) * ln(z' / z0).
"""

from __future__ import annotations

from typing import Union

import numpy as np

ArrayLike = Union[float, np.ndarray]

# von Karman constant (dimensionless), standard turbulent boundary-layer value.
VON_KARMAN = 0.41


def _as_array(x: ArrayLike) -> np.ndarray:
    return np.asarray(x, dtype=float)


def power_law_profile(
    height_above_bed: ArrayLike,
    total_depth: float,
    surface_velocity: float,
    exponent_m: float = 6.0,
) -> ArrayLike:
    """Open-channel power-law current velocity at a height above the bed.

    .. math::

        u(z') = u_s \\left( \\frac{z'}{H} \\right)^{1/m}

    where ``z'`` is the height above the river bed, ``H`` is the total flow
    depth, ``u_s`` is the surface velocity, and ``m`` is the power-law
    exponent (commonly 6 for natural rivers; 7 for the classic 1/7 law).

    The velocity is zero at the bed (``z' = 0``) and equals ``surface_velocity``
    at the free surface (``z' = total_depth``), increasing monotonically.

    Args:
        height_above_bed: Height above the river bed (m), 0 = bed,
            ``total_depth`` = surface. Scalar or array.
        total_depth: Total flow depth ``H`` (m), must be > 0.
        surface_velocity: Current speed at the free surface ``u_s`` (m/s).
        exponent_m: Power-law exponent ``m`` (dimensionless, > 0). Default 6.0.

    Returns:
        Current speed (m/s) at the requested height(s); same shape as input.

    Raises:
        ValueError: If ``total_depth <= 0`` or ``exponent_m <= 0``.
    """
    if total_depth <= 0:
        raise ValueError("total_depth must be > 0")
    if exponent_m <= 0:
        raise ValueError("exponent_m must be > 0")

    z = _as_array(height_above_bed)
    z = np.clip(z, 0.0, total_depth)
    speed = surface_velocity * (z / total_depth) ** (1.0 / exponent_m)
    if np.isscalar(height_above_bed):
        return float(speed)
    return speed


def power_law_depth_averaged(
    total_depth: float,
    surface_velocity: float,
    exponent_m: float = 6.0,
) -> float:
    """Depth-averaged velocity of the open-channel power-law profile.

    Analytical integral of :func:`power_law_profile` over ``z' in [0, H]``:

    .. math::

        \\bar{u} = \\frac{1}{H} \\int_0^H u_s (z'/H)^{1/m} \\, dz'
                 = u_s \\frac{m}{m + 1}

    Args:
        total_depth: Total flow depth ``H`` (m), must be > 0 (cancels out but
            validated for consistency).
        surface_velocity: Surface velocity ``u_s`` (m/s).
        exponent_m: Power-law exponent ``m`` (dimensionless, > 0).

    Returns:
        Depth-averaged current speed (m/s).

    Raises:
        ValueError: If ``total_depth <= 0`` or ``exponent_m <= 0``.
    """
    if total_depth <= 0:
        raise ValueError("total_depth must be > 0")
    if exponent_m <= 0:
        raise ValueError("exponent_m must be > 0")
    return surface_velocity * exponent_m / (exponent_m + 1.0)


def log_law_profile(
    height_above_bed: ArrayLike,
    shear_velocity: float,
    roughness_height: float,
    von_karman: float = VON_KARMAN,
) -> ArrayLike:
    """Logarithmic (law of the wall) river current velocity above the bed.

    .. math::

        u(z') = \\frac{u_*}{\\kappa} \\ln\\!\\left( \\frac{z'}{z_0} \\right)

    where ``u_*`` is the bed shear (friction) velocity, ``kappa`` is the von
    Karman constant, ``z_0`` is the bed roughness length, and ``z'`` is the
    height above the bed. The velocity is zero at ``z' = z_0`` and increases
    monotonically (logarithmically) toward the surface. For ``z' < z_0`` the
    log law is not valid; the velocity is clamped to 0.

    Args:
        height_above_bed: Height above the river bed (m). Scalar or array.
        shear_velocity: Bed shear/friction velocity ``u_*`` (m/s), > 0.
        roughness_height: Bed roughness length ``z_0`` (m), > 0.
        von_karman: von Karman constant ``kappa`` (default 0.41).

    Returns:
        Current speed (m/s) at the requested height(s); same shape as input.
        Values below ``z_0`` (where the log law gives <= 0) are clamped to 0.

    Raises:
        ValueError: If ``shear_velocity <= 0``, ``roughness_height <= 0``,
            or ``von_karman <= 0``.
    """
    if shear_velocity <= 0:
        raise ValueError("shear_velocity must be > 0")
    if roughness_height <= 0:
        raise ValueError("roughness_height must be > 0")
    if von_karman <= 0:
        raise ValueError("von_karman must be > 0")

    z = _as_array(height_above_bed)
    # Guard ln of non-positive ratios; clamp sub-roughness heights to z0.
    z_clamped = np.clip(z, roughness_height, None)
    speed = (shear_velocity / von_karman) * np.log(z_clamped / roughness_height)
    speed = np.maximum(speed, 0.0)
    if np.isscalar(height_above_bed):
        return float(speed)
    return speed


def log_law_depth_averaged(
    total_depth: float,
    shear_velocity: float,
    roughness_height: float,
    von_karman: float = VON_KARMAN,
) -> float:
    """Depth-averaged velocity of the logarithmic river profile.

    Analytical integral of :func:`log_law_profile` over ``z' in [z_0, H]``
    (the region where the log law is positive), divided by the total depth
    ``H``:

    .. math::

        \\bar{u} = \\frac{1}{H} \\int_{z_0}^{H}
                   \\frac{u_*}{\\kappa} \\ln(z'/z_0) \\, dz'
                 = \\frac{u_*}{\\kappa H}
                   \\left[ H \\ln(H/z_0) - (H - z_0) \\right]

    using :math:`\\int \\ln(z/z_0)\\,dz = z\\ln(z/z_0) - z`.

    Args:
        total_depth: Total flow depth ``H`` (m), must be > ``roughness_height``.
        shear_velocity: Bed shear/friction velocity ``u_*`` (m/s), > 0.
        roughness_height: Bed roughness length ``z_0`` (m), > 0.
        von_karman: von Karman constant ``kappa`` (default 0.41).

    Returns:
        Depth-averaged current speed (m/s).

    Raises:
        ValueError: If inputs are non-positive or ``total_depth <= z_0``.
    """
    if shear_velocity <= 0:
        raise ValueError("shear_velocity must be > 0")
    if roughness_height <= 0:
        raise ValueError("roughness_height must be > 0")
    if von_karman <= 0:
        raise ValueError("von_karman must be > 0")
    if total_depth <= roughness_height:
        raise ValueError("total_depth must be > roughness_height")

    H = total_depth
    z0 = roughness_height
    integral = H * np.log(H / z0) - (H - z0)
    return float((shear_velocity / (von_karman * H)) * integral)


def river_velocity_profile(
    n_points: int,
    total_depth: float,
    *,
    model: str = "power_law",
    surface_velocity: float = 1.0,
    exponent_m: float = 6.0,
    shear_velocity: float = 0.1,
    roughness_height: float = 0.01,
    von_karman: float = VON_KARMAN,
) -> tuple[np.ndarray, np.ndarray]:
    """Generate a discrete (height-above-bed, speed) river current profile.

    Convenience wrapper that samples a profile model at ``n_points`` evenly
    spaced heights from the bed (0) to the surface (``total_depth``).

    Args:
        n_points: Number of vertical sample points (>= 2).
        total_depth: Total flow depth ``H`` (m), > 0.
        model: ``"power_law"`` or ``"log_law"``.
        surface_velocity: Surface speed (m/s) for the power-law model.
        exponent_m: Power-law exponent for the power-law model.
        shear_velocity: Bed shear velocity (m/s) for the log-law model.
        roughness_height: Bed roughness length (m) for the log-law model.
        von_karman: von Karman constant for the log-law model.

    Returns:
        Tuple ``(heights, speeds)`` of equal-length 1-D arrays. ``heights`` is
        height above the bed (m, 0 = bed, ``total_depth`` = surface).

    Raises:
        ValueError: If ``n_points < 2`` or ``model`` is unknown.
    """
    if n_points < 2:
        raise ValueError("n_points must be >= 2")

    heights = np.linspace(0.0, total_depth, n_points)
    if model == "power_law":
        speeds = power_law_profile(
            heights, total_depth, surface_velocity, exponent_m=exponent_m
        )
    elif model == "log_law":
        speeds = log_law_profile(
            heights, shear_velocity, roughness_height, von_karman=von_karman
        )
    else:
        raise ValueError(f"unknown model: {model!r} (expected 'power_law' or 'log_law')")

    return heights, np.asarray(speeds, dtype=float)
