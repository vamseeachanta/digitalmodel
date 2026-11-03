# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Reduce supplied airfoil coefficients to an unweighted linear lift fit.

This module performs postprocessing only. It does not supply reference data,
infer a linear aerodynamic regime, or establish native solver validation.
The caller declares the fitting interval and supplies dimensionless Cl and Cd.

The fit minimizes sum((Cl - (intercept + slope * angle))**2). Centering gives
slope = sum(dx * dy) / sum(dx**2) and intercept = mean(Cl) - slope * mean(angle),
from the two linear least-squares normal equations. The implementation centers
sample-origin offsets and scales dx before summation, retaining those offsets
for fitted values and reconstructing the intercept in the original coordinates.
Basis: NIST/SEMATECH e-Handbook of Statistical Methods (2012 edition),
section 4.4.3.1, "Least Squares":
https://www.itl.nist.gov/div898/handbook/pmd/section4/pmd431.htm
Unit conversion uses pi radians = 180 degrees by definition: the slope per
radian equals the slope per degree multiplied by 180 / pi. Basis: BIPM,
The International System of Units, 9th edition (2019), section 4, Table 8:
https://www.bipm.org/en/publications/si-brochure

Saved example (synthetic hand calculation: Cl = 0.2 + 0.1 * angle_deg):

>>> result = reduce_airfoil_polar(
...     [-2, 0, 2], [0, 0.2, 0.4], [0.02, 0.02, 0.02],
...     angle_units="degrees", fit_interval=(-2, 2),
... )
>>> print(f"slope per degree: {result.lift_slope_per_degree:.1f}")
slope per degree: 0.1
>>> print(f"intercept: {result.lift_intercept:.1f}")
intercept: 0.2
>>> result.selected_indices
(0, 1, 2)
"""

from __future__ import annotations

from dataclasses import dataclass
from math import fsum, isfinite, pi
from typing import Literal, Sequence


@dataclass(frozen=True)
class AirfoilPolarResult:
    """Lift fit and its selected samples, all retained in input order.

    Angles and interval bounds use ``angle_units``. Indices refer to the
    original inputs. Residuals are observed Cl minus fitted Cl, dimensionless
    like the intercept. Cd is retained for polar plotting and is not fitted.
    """

    angle_units: Literal["degrees", "radians"]
    fit_interval: tuple[float, float]
    selected_indices: tuple[int, ...]
    selected_angles: tuple[float, ...]
    selected_cl: tuple[float, ...]
    selected_cd: tuple[float, ...]
    lift_slope_per_radian: float
    lift_slope_per_degree: float
    lift_intercept: float
    fitted_cl: tuple[float, ...]
    residuals: tuple[float, ...]


def _finite_values(values: Sequence[float], name: str) -> tuple[float, ...]:
    """Copy a numeric sequence and reject malformed or nonfinite values."""
    try:
        result = tuple(float(value) for value in values)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError(f"{name} must be a sequence of finite real numbers") from exc
    if not all(isfinite(value) for value in result):
        raise ValueError(f"{name} must contain only finite real numbers")
    return result


def reduce_airfoil_polar(
    angles: Sequence[float],
    cl: Sequence[float],
    cd: Sequence[float],
    *,
    angle_units: Literal["degrees", "radians"],
    fit_interval: tuple[float, float],
) -> AirfoilPolarResult:
    """Fit Cl against angle inside an explicitly declared closed interval.

    Args:
        angles: Distinct finite sample angles, in any order.
        cl: Finite dimensionless lift coefficients paired with ``angles``.
        cd: Finite nonnegative dimensionless drag coefficients paired with
            ``angles``. Signed axial force coefficients must be converted
            to this drag convention before reduction; zero drag is allowed.
        angle_units: Exactly ``"degrees"`` or ``"radians"``; never inferred.
        fit_interval: Strictly increasing finite lower and upper bounds in
            ``angle_units``. Both endpoints are included, without a tolerance.

    Returns:
        An immutable result containing both lift-slope units, the intercept,
        selected samples, fitted Cl and observed-minus-fitted residuals.
        Inputs are copied; sample ordering is preserved.

    Raises:
        ValueError: If units, shape, lengths, finiteness, uniqueness or drag
            signs violate the contract, fewer than two points are selected,
            or the fit cannot be represented with finite floating point values.
            All input samples are validated, including those outside the interval.
    """
    if angle_units not in ("degrees", "radians"):
        raise ValueError('angle_units must be "degrees" or "radians"')
    angles = _finite_values(angles, "angles")
    cl = _finite_values(cl, "cl")
    cd = _finite_values(cd, "cd")
    if not len(angles) == len(cl) == len(cd):
        raise ValueError("angles, cl and cd must have equal length")
    if len(set(angles)) != len(angles):
        raise ValueError("angles must be distinct")
    if any(value < 0 for value in cd):
        raise ValueError("cd must be nonnegative")
    bounds = _finite_values(fit_interval, "fit_interval")
    if len(bounds) != 2:
        raise ValueError("fit_interval must contain exactly two bounds")
    lower, upper = bounds
    if lower >= upper:
        raise ValueError("fit_interval bounds must be strictly increasing")

    indices = tuple(i for i, angle in enumerate(angles) if lower <= angle <= upper)
    # A line has two unknown parameters: slope and intercept.
    if len(indices) < 2:
        raise ValueError("fit_interval must select at least two distinct angles")
    x = tuple(angles[i] for i in indices)
    y = tuple(cl[i] for i in indices)
    drag = tuple(cd[i] for i in indices)

    try:
        x_origin = x[0]
        offsets = tuple(value - x_origin for value in x)
        if not all(isfinite(value) for value in offsets):
            # Opposite-sign extremes can overflow a sample-origin subtraction.
            x_origin = 0.0
            offsets = x
        offset_mean = fsum(value / len(x) for value in offsets)
        y_mean = fsum(value / len(y) for value in y)
        dx = tuple(value - offset_mean for value in offsets)
        scale = max(abs(value) for value in dx)
        normalized = tuple(value / scale for value in dx)
        slope = (
            fsum(a * (b - y_mean) for a, b in zip(normalized, y))
            / fsum(a * a for a in normalized)
            / scale
        )
        intercept = fsum((y_mean, -slope * offset_mean, -slope * x_origin))
        fitted = tuple(y_mean + slope * delta for delta in dx)
        residuals = tuple(observed - fit for observed, fit in zip(y, fitted))
        radians_per_degree = pi / 180  # Exact angle-unit definition above.
        per_radian = (
            slope / radians_per_degree if angle_units == "degrees" else slope
        )
        per_degree = slope if angle_units == "degrees" else slope * radians_per_degree
        values = (intercept, per_radian, per_degree, *fitted, *residuals)
        if not all(isfinite(value) for value in values):
            raise ValueError("nonfinite fit")
    except (ArithmeticError, ValueError) as exc:
        raise ValueError(
            "fit cannot be represented with finite floating point values"
        ) from exc

    return AirfoilPolarResult(
        angle_units=angle_units,
        fit_interval=(lower, upper),
        selected_indices=indices,
        selected_angles=x,
        selected_cl=y,
        selected_cd=drag,
        lift_slope_per_radian=per_radian,
        lift_slope_per_degree=per_degree,
        lift_intercept=intercept,
        fitted_cl=fitted,
        residuals=residuals,
    )
