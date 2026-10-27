# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Explicit design fatigue factor selection and calculated life requirements.

The defaults 1, 2, 3 and 10 are common practice values, not verified code
requirements. The user must confirm the category and factor against the
governing code before use. No standard table is reproduced here.

Accessible categories assume access for both inspection and repair during
service. Use NOT_ACCESSIBLE when either is unavailable, for example at a
splash zone or seabed location without practical access. Where failure has
substantial consequences, select SUBSTANTIAL_CONSEQUENCE regardless of access.
Location alone does not establish inspectability or repairability.

For example, a 25-year service life with NOT_ACCESSIBLE's default factor 3
requires 75 years of calculated fatigue life::

    factor = select_design_fatigue_factor(DFFCategory.NOT_ACCESSIBLE)
    target_years = required_calculated_life(25.0, factor)

Pass target_years to damage.design_life_check as target_years. Apply the factor
once; the target is required unfactored calculated life, not service life.
"""

import math
from collections.abc import Mapping
from enum import Enum
from numbers import Real
from types import MappingProxyType


class DFFCategory(str, Enum):
    """Categories chosen by the user from access and failure criticality."""

    ACCESSIBLE_NON_CRITICAL = "accessible_non_critical"
    ACCESSIBLE_CRITICAL = "accessible_critical"
    NOT_ACCESSIBLE = "not_accessible"
    SUBSTANTIAL_CONSEQUENCE = "substantial_consequence"


_DEFAULT_FACTORS: Mapping[DFFCategory, float] = MappingProxyType(
    {
        DFFCategory.ACCESSIBLE_NON_CRITICAL: 1.0,
        DFFCategory.ACCESSIBLE_CRITICAL: 2.0,
        DFFCategory.NOT_ACCESSIBLE: 3.0,
        DFFCategory.SUBSTANTIAL_CONSEQUENCE: 10.0,
    }
)


def _finite_real(value: float, name: str) -> float:
    """Reject non-numeric, boolean and non-finite input values."""
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite real number")
    try:
        result = float(value)
    except OverflowError as exc:
        raise ValueError(f"{name} must be a finite real number") from exc
    if not math.isfinite(result):
        raise ValueError(f"{name} must be a finite real number")
    return result


def select_design_fatigue_factor(
    category: DFFCategory | str,
    *,
    factors: Mapping[DFFCategory, float] | None = None,
) -> float:
    """Return the selected factor, with optional per-category overrides.

    Args:
        category: Explicit access and consequence category, or its string value.
        factors: Overrides for selected categories; omitted categories retain
            common practice defaults. The mapping is not modified.

    Returns:
        The factor for the selected category, to be confirmed by the user
        against the governing code.

    Raises:
        ValueError: The category is unknown or its factor is non-finite or < 1.
    """
    selected = DFFCategory(category)
    value = _DEFAULT_FACTORS[selected]
    if factors is not None:
        value = factors.get(selected, value)
    factor = _finite_real(value, "design fatigue factor")
    if factor < 1.0:
        raise ValueError("design fatigue factor must be >= 1")
    return factor


def required_calculated_life(service_life_years: float, factor: float) -> float:
    """Return service life multiplied by a supplied design fatigue factor.

    Args:
        service_life_years: Finite positive service duration in years.
        factor: Finite design fatigue factor >= 1, selected and confirmed
            against the governing code by the user.

    Returns:
        Required calculated fatigue life in years, with the factor applied once.

    Raises:
        ValueError: An input is invalid or the calculated life overflows.
    """
    years = _finite_real(service_life_years, "service_life_years")
    if years <= 0:
        raise ValueError("service_life_years must be > 0")
    factor = _finite_real(factor, "design fatigue factor")
    if factor < 1.0:
        raise ValueError("design fatigue factor must be >= 1")
    required_years = years * factor
    if not math.isfinite(required_years):
        raise ValueError("required calculated life must be finite")
    return required_years
