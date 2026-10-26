# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Perry-Robertson first-yield strength of a bowed, uniform pinned strut.

Textbook basis: E. R. Johnson, *Aerospace Structures* (2022), section 11.4,
"Geometrically imperfect column", equations 11.77 and 11.83-11.85:
https://eng.libretexts.org/Under_Construction/Aerospace_Structures_(Johnson)/11%3A_Buckling_of_columns_and_plates
Page to be confirmed by the maintainer. The textbook supplies the elastic
initial-bow solution; the first-yield quadratic is derived below rather than
attributed to an unseen textbook equation.

Historical reference: A. Robertson, "The strength of struts" (1925),
Selected Engineering Papers, 1(28), https://doi.org/10.1680/isenp.1925.15138.
Bibliographic metadata confirmed; paper text not used. Page to be confirmed
by the maintainer. No empirical imperfection coefficient is taken from it.

Derivation (compression positive; small slopes; elastic until first yield):
For initial bow v0=e0*sin(pi*z/L), the textbook solution gives total midspan
bow v=e0/(1-P/PE), PE=pi^2*E*I/L^2, and moment M=P*v. With r^2=I/A,
c the extreme-fibre distance in the bending direction, and sigma=P/A,
the extreme-fibre stress is sigma_max=sigma+M*c/I
=sigma*[1+eta/(1-sigma/sigma_E)], where eta=e0*c/r^2 and sigma_E=PE/A.
Set sigma_max=fy, q=sigma/fy and lambda_bar=sqrt(fy/sigma_E):

    (1-q)*(1-q*lambda_bar^2) = eta*q
    lambda_bar^2*q^2 - (1+eta+lambda_bar^2)*q + 1 = 0.

The smaller root, rationalised to avoid subtractive cancellation, is
q=2/[s+sqrt(s^2-4*lambda_bar^2)], s=1+eta+lambda_bar^2.
At eta=0 its continuous extension is min(1, 1/lambda_bar^2), with q=1
at lambda_bar=0. At fixed nonzero eta the short-column limit is 1/(1+eta),
not 1. The squash limit requires eta to vanish, as when e0/L and c/r are
fixed while L/r tends to zero: eta=(e0/L)*(c/r)*(L/r).

This is a first-yield model for global flexural buckling. It does not predict
plastic redistribution, residual stress, local plate buckling, torsional or
flexural-torsional buckling, connection restraint or design acceptance.
The caller supplies section properties and imperfection; no default bow,
empirical calibration or safety factor is implicit.
"""

from math import hypot, isfinite, pi, sqrt


def _nonnegative(name: str, value: float) -> None:
    if not isfinite(value) or value < 0:
        raise ValueError(f"{name} must be finite and nonnegative")


def perry_robertson_strength_ratio(
    *, reduced_slenderness: float, imperfection_parameter: float
) -> float:
    """Return mean first-yield stress divided by compressive yield strength.

    Args:
        reduced_slenderness: Dimensionless sqrt(fy/sigma_E), nonnegative.
        imperfection_parameter: Dimensionless eta=e0*c/r^2, nonnegative.

    Returns:
        A positive ratio no greater than the squash or Euler limit.

    Raises:
        ValueError: If inputs are invalid or an intermediate or result is
            outside the floating-point range. Zero slenderness is a limit.
    """
    _nonnegative("reduced_slenderness", reduced_slenderness)
    _nonnegative("imperfection_parameter", imperfection_parameter)
    eta = imperfection_parameter
    try:
        slenderness_squared = reduced_slenderness**2
        s = 1.0 + eta + slenderness_squared
        # Equivalent to sqrt(s*s-4*lambda_bar^2), retaining tiny positive
        # imperfections near lambda_bar=1 without cancellation.
        discriminant_root = hypot(
            1.0 - slenderness_squared,
            sqrt(eta) * sqrt(2.0 * (1.0 + slenderness_squared) + eta),
        )
        ratio = 1.0 / (0.5 * s + 0.5 * discriminant_root)
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("strength ratio is outside the floating-point range") from exc
    if not isfinite(s) or not isfinite(ratio) or ratio <= 0:
        raise ValueError("strength ratio is outside the floating-point range")
    return ratio


def perry_robertson_stress(
    *,
    youngs_modulus: float,
    yield_strength: float,
    slenderness_ratio: float,
    imperfection_parameter: float,
) -> float:
    """Return mean first-yield stress in the units of the material inputs.

    Args:
        youngs_modulus: Positive finite elastic modulus E.
        yield_strength: Positive finite compressive yield stress fy, same unit as E.
        slenderness_ratio: Nonnegative L/r for the pinned strut, dimensionless.
        imperfection_parameter: Explicit eta=e0*c/r^2, dimensionless.

    Returns:
        Positive mean axial stress, equal to fy times the strength ratio.

    Raises:
        ValueError: If inputs are invalid or the calculation is unrepresentable.

    For other end conditions, any equivalent length and bow model must be
    justified separately. The zero-slenderness input represents a limit.
    """
    for name, value in (
        ("youngs_modulus", youngs_modulus), ("yield_strength", yield_strength)
    ):
        if not isfinite(value) or value <= 0:
            raise ValueError(f"{name} must be finite and positive")
    _nonnegative("slenderness_ratio", slenderness_ratio)
    reduced_slenderness = slenderness_ratio / pi * (
        sqrt(yield_strength) / sqrt(youngs_modulus)
    )
    if not isfinite(reduced_slenderness) or (
        slenderness_ratio > 0 and reduced_slenderness == 0
    ):
        raise ValueError("reduced slenderness is outside the floating-point range")
    ratio = perry_robertson_strength_ratio(
        reduced_slenderness=reduced_slenderness,
        imperfection_parameter=imperfection_parameter,
    )
    stress = yield_strength * ratio
    if not isfinite(stress) or stress <= 0:
        raise ValueError("critical stress is outside the floating-point range")
    return stress
