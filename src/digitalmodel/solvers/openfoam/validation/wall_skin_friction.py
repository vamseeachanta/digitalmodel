# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reduce a signed wall-shear profile using the dynamic-pressure definition.

The normalization is Cf = tau / q with q = rho U² / 2. The factor of one half
is part of the definition of dynamic pressure, not an empirical constant.
This helper performs no solver calls, turbulence modelling or reference fit.

Source: NASA, CFPOST User's Guide, Wind-US 3.0 documentation, Appendix C,
"Equations Used by CFPOST", "Variable Derivations" entries "q" and "Cf":
https://www.grc.nasa.gov/www/winddocs/windus3.0/cfpost/appc.html
The component definitions apply to the signed streamwise shear used here.
"""

from __future__ import annotations

import math

import numpy as np
from numpy.typing import ArrayLike, NDArray


def skin_friction_profile(
    x: ArrayLike,
    wall_shear: ArrayLike,
    *,
    density: float,
    speed: float,
    developed_mask: ArrayLike | None = None,
) -> tuple[NDArray[np.float64], NDArray[np.float64]]:
    """Return streamwise coordinates and signed local skin-friction coefficients.

    Args:
        x: Nonempty, finite, one-dimensional streamwise coordinates in metres.
            Their order and repeated coordinates are preserved.
        wall_shear: Matching signed streamwise shear stresses in Pa. The caller
            projects vector shear onto the desired streamwise direction and
            sets its sign convention. Kinematic stresses must first be
            converted to Pa; neither magnitudes nor unit conversion are inferred.
        density: Explicit finite positive reference density in kg/m³.
        speed: Explicit finite positive reference speed in m/s.
        developed_mask: Optional Boolean vector matching the full input shape.
            True selects a row; at least one row must be selected. Every input
            row is validated before selection, including excluded rows.

    Returns:
        Independent float arrays ``(x, cf)`` for the selected rows, with
        ``cf = wall_shear / (density * speed² / 2)``. Inputs are not modified.

    Raises:
        ValueError: If profiles or the mask have invalid shape, samples are
            nonfinite, references are not positive finite scalars, or dynamic
            pressure or coefficients cannot be represented as finite floats,
            or a coefficient from nonzero shear rounds to zero.

    Example:
        Synthetic hand balance: rho = 2 kg/m³, U = 4 m/s gives q = 16 Pa.
        Shears of 8 and -4 Pa therefore give Cf of 0.5 and -0.25.

        >>> x, cf = skin_friction_profile(
        ...     [1.0, 2.0], [8.0, -4.0], density=2.0, speed=4.0
        ... )
        >>> x.tolist(), cf.tolist()
        ([1.0, 2.0], [0.5, -0.25])
    """
    coordinates = _finite_profile(x, "x")
    shear = _finite_profile(wall_shear, "wall_shear")
    if coordinates.shape != shear.shape:
        raise ValueError("x and wall_shear must have matching shapes")

    rho = _positive_scalar(density, "density")
    velocity = _positive_scalar(speed, "speed")
    # Keep intermediate products normal; apply the combined exponent once.
    rho_mantissa, rho_exponent = math.frexp(rho)
    speed_mantissa, speed_exponent = math.frexp(velocity)
    try:
        dynamic_pressure = math.ldexp(
            rho_mantissa * speed_mantissa * speed_mantissa,
            rho_exponent + 2 * speed_exponent - 1,
        )
    except OverflowError as exc:
        raise ValueError("dynamic pressure must be finite and positive") from exc
    if not math.isfinite(dynamic_pressure) or dynamic_pressure <= 0.0:
        raise ValueError("dynamic pressure must be finite and positive")

    with np.errstate(over="ignore", under="ignore", invalid="ignore"):
        cf = shear / dynamic_pressure
    if not np.isfinite(cf).all() or np.any((shear != 0.0) & (cf == 0.0)):
        raise ValueError(
            "skin-friction coefficients must be finite and nonzero for nonzero shear"
        )

    if developed_mask is None:
        return coordinates.copy(), cf
    mask = np.asarray(developed_mask)
    if mask.dtype != np.bool_ or mask.shape != coordinates.shape:
        raise ValueError("developed_mask must be a matching Boolean vector")
    if not mask.any():
        raise ValueError("developed_mask must select at least one row")
    return coordinates[mask], cf[mask]


def _finite_profile(values: ArrayLike, name: str) -> NDArray[np.float64]:
    """Validate all samples without flattening or removing invalid rows."""
    if np.iscomplexobj(values):
        raise ValueError(f"{name} must contain real finite samples")
    array = np.asarray(values, dtype=np.float64)
    if array.ndim != 1 or array.size == 0:
        raise ValueError(f"{name} must be a nonempty one-dimensional profile")
    if not np.isfinite(array).all():
        raise ValueError(f"{name} must contain only finite samples")
    return array


def _positive_scalar(value: float, name: str) -> float:
    """Reject array references rather than implicitly broadcasting them."""
    if np.ndim(value) != 0 or np.iscomplexobj(value):
        raise ValueError(f"{name} must be a finite positive scalar")
    scalar = float(value)
    if not math.isfinite(scalar) or scalar <= 0.0:
        raise ValueError(f"{name} must be a finite positive scalar")
    return scalar
