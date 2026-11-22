"""Scalar validation; quantities use the shared digitalmodel Pint registry."""

import math

from digitalmodel.units import Q_


def scalar(value, unit: str, name: str, *, minimum=0.0, positive=False) -> float:
    """Convert a quantity to a finite scalar; bare values use the stated unit."""
    number = float(
        value.to(unit).magnitude if hasattr(value, "to") else Q_(value, unit).magnitude
    )
    if (
        not math.isfinite(number)
        or number < minimum
        or (positive and number == minimum)
    ):
        relation = ">" if positive else ">="
        raise ValueError(f"{name} must be finite and {relation} {minimum} {unit}")
    return number


def finite(value: float, name: str, *, minimum=0.0, maximum=None) -> float:
    """Validate a dimensionless scalar, optionally with an upper bound."""
    number = scalar(value, "dimensionless", name, minimum=minimum)
    if maximum is not None and number > maximum:
        raise ValueError(f"{name} must be <= {maximum}")
    return number
