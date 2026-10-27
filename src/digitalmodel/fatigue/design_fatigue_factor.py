# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Explicit design fatigue factor selection and calculated life requirements.

The common practice defaults 1, 2, 3 and 10 are not verified code requirements.
They are reference values only: no factor is returned by default. The user must
explicitly configure and confirm the category and factor against the governing
code, with a resolvable Citation and applicability recorded in Citation.note.
No standard table is reproduced here.

Accessible categories assume access for both inspection and repair during
service. Use NOT_ACCESSIBLE when either is unavailable, for example at a
splash zone or seabed location without practical access. Where failure has
substantial consequences, select SUBSTANTIAL_CONSEQUENCE regardless of access.
Location alone does not establish inspectability or repairability.

For example, if the user has confirmed factor 3 for NOT_ACCESSIBLE against the
governing code, a 25-year service life requires 75 years of calculated fatigue
life. With ``confirmed_citation`` identifying that basis and its applicability::

    from digitalmodel.citations import CitedValue

    factors = {DFFCategory.NOT_ACCESSIBLE: CitedValue(3.0, confirmed_citation)}
    factor = select_design_fatigue_factor(DFFCategory.NOT_ACCESSIBLE, factors=factors)
    target_years = required_calculated_life(25.0, factor)

Pass target_years to damage.design_life_check as target_years. Apply the factor
once; the target is required unfactored calculated life, not service life.
"""

import math
from collections.abc import Mapping
from enum import Enum
from numbers import Real
from pathlib import Path

from digitalmodel.citations import Citation, CitedValue, validate_citation


class DFFCategory(str, Enum):
    """Categories chosen by the user from access and failure criticality."""

    ACCESSIBLE_NON_CRITICAL = "accessible_non_critical"
    ACCESSIBLE_CRITICAL = "accessible_critical"
    NOT_ACCESSIBLE = "not_accessible"
    SUBSTANTIAL_CONSEQUENCE = "substantial_consequence"


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
    factors: Mapping[DFFCategory, CitedValue] | None = None,
    repo_root: Path | None = None,
) -> float:
    """Return an explicitly configured factor after validating its citation.

    Args:
        category: Explicit access and consequence category, or its string value.
        factors: User-confirmed, dimensionless factors with a Citation for each
            configured category. Citation.note records the applicability to the
            selected category, including inspection, repair and consequences.
            A missing category is rejected. The mapping is not modified.
        repo_root: Optional citation repository root; otherwise use the existing
            citation resolver. Resolution is checked on every selection.

    Returns:
        The user-supplied factor for the selected category. Citation resolution
        checks source identity and revision, not the engineering interpretation;
        the user remains responsible for confirming governing-code suitability.

    Raises:
        ValueError: The category, factor, citation or applicability is missing or
            invalid, the factor has units, or its value is non-finite or < 1.
        CitationResolutionError: The cited page is missing or its source identity
            or revision does not match.
    """
    selected = DFFCategory(category)
    if factors is None or selected not in factors:
        raise ValueError("an explicit design fatigue factor is required for the category")
    value = factors[selected]
    if not isinstance(value, CitedValue) or not isinstance(value.citation, Citation):
        raise ValueError("design fatigue factor requires a CitedValue with a citation")
    note = value.citation.note
    if not isinstance(note, str) or not note.strip():
        raise ValueError("design fatigue factor citation requires an applicability note")
    if value.units not in ("", "1"):
        raise ValueError("design fatigue factor must be dimensionless")
    factor = _finite_real(value.value, "design fatigue factor")
    if factor < 1.0:
        raise ValueError("design fatigue factor must be >= 1")
    validate_citation(value.citation, repo_root=repo_root)
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
