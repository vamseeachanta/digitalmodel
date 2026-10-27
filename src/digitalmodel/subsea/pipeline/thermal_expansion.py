# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Free thermal growth from uniform linear thermal strain.

For an unrestrained reference length L and a constant linear coefficient alpha,
the uniform engineering strain is epsilon = alpha * delta_T. Integrating along
the reference length gives delta_L = integral(epsilon dx, x=0..L)
= L * alpha * delta_T. This is a small-strain model with uniform temperature
change and no pressure, restraint, friction, buckling or walking calculation.

Basis: OpenStax, University Physics Volume 2, 2016 edition, section 1.3,
"Thermal Expansion", equation 1.2 (linear thermal expansion).
https://openstax.org/books/university-physics-volume-2/pages/1-3-thermal-expansion
"""

from math import frexp, isfinite, ldexp
from numbers import Real


def free_thermal_expansion(
    length: float,
    thermal_expansion_coefficient: float,
    temperature_change: float,
) -> float:
    """Return signed free growth in the same length unit as the input.

    Args:
        length: Nonnegative reference length in a consistent length unit.
        thermal_expansion_coefficient: Explicit nonnegative linear coefficient
            per temperature unit, constant over the specified change.
        temperature_change: Final minus initial temperature, in the reciprocal
            unit of the coefficient. A negative change represents cooling.

    Returns:
        Positive growth for heating or negative growth for cooling. Zero length,
        coefficient or temperature change gives zero growth.

    Raises:
        TypeError: An input is a boolean or is not a real scalar.
        ValueError: An input is not finite in floating-point representation,
            length or coefficient is negative, growth is not finite, or
            a nonzero input or growth underflows to zero.
    """
    inputs = {
        "length": length,
        "thermal_expansion_coefficient": thermal_expansion_coefficient,
        "temperature_change": temperature_change,
    }
    for name, value in inputs.items():
        if isinstance(value, bool) or not isinstance(value, Real):
            raise TypeError(f"{name} must be a real scalar, excluding booleans")
        if name in ("length", "thermal_expansion_coefficient") and value < 0:
            raise ValueError(f"{name} must be nonnegative")
        try:
            inputs[name] = float(value)
        except OverflowError as exc:
            raise ValueError(f"{name} must be finite") from exc
        if not isfinite(inputs[name]):
            raise ValueError(f"{name} must be finite")
        if inputs[name] == 0.0 and value != 0:
            raise ValueError(f"{name} must remain nonzero in floating point")

    # A zero integration interval or zero strain gives zero growth even if
    # multiplying the other two finite inputs would overflow.
    if any(value == 0.0 for value in inputs.values()):
        return 0.0

    # Separate powers of two so a partial product cannot lose finite growth.
    # Each nonzero mantissa has magnitude in [0.5, 1), so three fit safely.
    mantissa, exponent = 1.0, 0
    for value in inputs.values():
        fraction, power = frexp(value)
        mantissa *= fraction
        exponent += power
    try:
        growth = ldexp(mantissa, exponent)
    except OverflowError as exc:
        raise ValueError("growth must be finite") from exc
    if not isfinite(growth):
        raise ValueError("growth must be finite")
    if growth == 0.0:
        raise ValueError("nonzero growth must be representable")
    return growth
