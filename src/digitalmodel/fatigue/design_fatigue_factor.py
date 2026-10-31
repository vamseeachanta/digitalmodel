# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Explicit design fatigue factor selection and calculated life requirements.

No default factors are supplied. The caller provides the selected category's
factor and Citation, including the governing edition and clause. Selection
validates the citation against the source page at calculation time. The caller
retains the citation mapping as the numeric result's provenance sidecar.

Accessible categories assume access for both inspection and repair during
service. Use NOT_ACCESSIBLE when either is unavailable, for example at a
splash zone or seabed location without practical access. Where failure has
substantial consequences, select SUBSTANTIAL_CONSEQUENCE regardless of access.
Location alone does not establish inspectability or repairability.

With caller-supplied factor and citation mappings::

    factor = select_design_fatigue_factor(
        DFFCategory.NOT_ACCESSIBLE, factors=factors, citations=citations
    )
    target_years = required_calculated_life(25.0, factor)

Source metadata resolution does not establish the correctness of the factor or
its applicability; the caller must confirm both against the governing code.
Pass target_years to damage.design_life_check as target_years. Apply the factor
once; the target is required unfactored calculated life, not service life.
"""

import math
from collections.abc import Mapping
from enum import Enum
from numbers import Real

from digitalmodel.citations import Citation, validate_citation


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
    factors: Mapping[DFFCategory, float] | None = None,
    citations: Mapping[DFFCategory, Citation] | None = None,
) -> float:
    """Return an explicit factor after resolving its supplied citation.

    Args:
        category: Explicit access and consequence category, or its string value.
        factors: Explicit factors; the selected category must be present.
        citations: Citation sidecars keyed by category; the selected category
            must have a Citation. Neither mapping is modified.

    Returns:
        The factor for the selected category. Source metadata is validated;
        factor correctness and applicability remain the caller's responsibility.

    Raises:
        ValueError: The category, explicit factor or citation is missing or
            invalid, or the factor is non-finite or < 1.
        CitationResolutionError: The source is missing or its metadata differs
            from the supplied citation.
    """
    selected = DFFCategory(category)
    if factors is None or selected not in factors:
        raise ValueError("an explicit design fatigue factor is required for the category")
    factor = _finite_real(factors[selected], "design fatigue factor")
    if factor < 1.0:
        raise ValueError("design fatigue factor must be >= 1")
    citation = citations.get(selected) if citations is not None else None
    if not isinstance(citation, Citation):
        raise ValueError("a citation is required for the selected design fatigue factor")
    validate_citation(citation)
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
