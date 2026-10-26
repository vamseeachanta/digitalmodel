# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Seawater conductivity and resistivity from surface-pressure PSS-78.

References:
    UNESCO (1983), *Algorithms for computation of fundamental properties of
    seawater*, Technical Papers in Marine Science 44, pp. 6-9, equations (1)-(5).
    This is the computational form of the Practical Salinity Scale 1978
    published in UNESCO Technical Papers in Marine Science 36/37 (1981).
    Open copy: https://repository.oceanbestpractices.org/handle/11329/109

    TEOS-10 (2011), *Notes on the GSW function gsw_SP_from_C*, p. 1,
    gives C(35, 15, 0) = 42.914 mS/cm = 4.2914 S/m, citing the standard
    conductivity measurement of Culkin and Smith (1980).
    https://www.teos-10.org/pubs/gsw/pdf/SP_from_C.pdf

Temperatures are degrees Celsius on IPTS-68, the scale of the published
equations, not ITS-90. Practical salinity is dimensionless; the parameter
suffix ``psu`` denotes the conventional practical-salinity notation, not a
mass fraction. Validity is 2 through 42 in salinity and -2 through 35 degrees C
(UNESCO 1983, pp. 7-8). Pressure is fixed at zero sea pressure (surface,
one standard atmosphere), as defined on p. 6. No depth correction is applied.
"""

from __future__ import annotations

import math

# UNESCO (1983), p. 7, equations (1)-(2); ascending powers of sqrt(R_t).
_A = (0.0080, -0.1692, 25.3851, 14.0941, -7.0261, 2.7081)
_B = (0.0005, -0.0056, -0.0066, -0.0375, 0.0636, -0.0144)
_K = 0.0162
# UNESCO (1983), pp. 7-8, equation (3); ascending powers of IPTS-68 t.
_C = (0.6766097, 2.00564e-2, 1.104259e-4, -6.9698e-7, 1.0031e-9)
# TEOS-10 (2011), Notes on gsw_SP_from_C, p. 1; SI conductivity [S/m].
_STANDARD_CONDUCTIVITY_S_M = 4.2914
# UNESCO (1983), pp. 6-8: reference temperature and closed working limits.
_REFERENCE_TEMPERATURE_C = 15.0
_SALINITY_LIMITS = (2.0, 42.0)
_TEMPERATURE_LIMITS_C = (-2.0, 35.0)


def _polynomial(coefficients: tuple[float, ...], x: float) -> float:
    """Evaluate ascending polynomial coefficients with Horner's method."""
    value = coefficients[-1]
    for coefficient in reversed(coefficients[:-1]):
        value = value * x + coefficient
    return value


def seawater_conductivity_s_m(salinity_psu: float, temperature_c: float) -> float:
    """Return conductivity [S/m] from practical salinity and IPTS-68 temperature.

    Numerically invert UNESCO (1983), pp. 6-9, equations (1)-(5), at zero
    sea pressure. The pressure factor R_p is unity, so R = R_t * r_t(t).
    Bisection in sqrt(R_t) avoids a derivative or an external solver.

    Args:
        salinity_psu: Finite practical salinity in the inclusive range 2 to 42.
        temperature_c: Finite temperature in the inclusive range -2 to 35
            degrees Celsius on IPTS-68. Convert ITS-90 measurements before use.

    Returns:
        Electrical conductivity in siemens per metre at surface pressure.

    Raises:
        ValueError: Either input is non-finite or outside the published range.
    """
    for name, value, limits in (
        ("salinity_psu", salinity_psu, _SALINITY_LIMITS),
        ("temperature_c", temperature_c, _TEMPERATURE_LIMITS_C),
    ):
        if not math.isfinite(value) or not limits[0] <= value <= limits[1]:
            raise ValueError(
                f"{name} must be finite and within [{limits[0]}, {limits[1]}]"
            )

    delta_t = temperature_c - _REFERENCE_TEMPERATURE_C
    correction = delta_t / (1.0 + _K * delta_t)
    coefficients = tuple(a + correction * b for a, b in zip(_A, _B))

    # Numerical choices, not empirical constants: sqrt(R_t) in [0, 2]
    # brackets the physical root throughout the cited domain. Sixty-four
    # halvings give a bracket below binary64 resolution; stop at stagnation.
    lower, upper = 0.0, 2.0
    for _ in range(64):
        midpoint = (lower + upper) / 2.0
        if midpoint == lower or midpoint == upper:
            break
        if _polynomial(coefficients, midpoint) < salinity_psu:
            lower = midpoint
        else:
            upper = midpoint
    root = (lower + upper) / 2.0
    temperature_ratio = _polynomial(_C, temperature_c)
    return _STANDARD_CONDUCTIVITY_S_M * temperature_ratio * root * root


def seawater_resistivity_ohm_m(salinity_psu: float, temperature_c: float) -> float:
    """Return surface seawater resistivity [ohm.m] using the PSS-78 inverse.

    Resistivity is the reciprocal of conductivity in S/m (SI unit identity).
    Input ranges, temperature scale, references and ValueError behaviour are
    those of :func:`seawater_conductivity_s_m`.
    """
    return 1.0 / seawater_conductivity_s_m(salinity_psu, temperature_c)
