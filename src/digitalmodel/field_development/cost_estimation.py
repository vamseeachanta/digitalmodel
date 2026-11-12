# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Component cost methods with caller-supplied, cited engineering inputs.

No default costs or empirical factors are supplied. Topsides cost is defined as
weight in tonnes multiplied by a USD/tonne rate and a dimensionless complexity
factor. Both cost inputs require citations resolved at every calculation.
The USD total retains the caller-supplied price base year.
Citation resolution checks source metadata, not market calibration or whether a
particular value is supported by the cited section; that remains a basis review.
"""

from dataclasses import dataclass
from datetime import MAXYEAR, MINYEAR
from math import isfinite
from numbers import Integral, Real

from digitalmodel.citations.schema import (
    Citation,
    CitationResolutionError,
    CitedValue,
    validate_citation,
)


@dataclass(frozen=True)
class TopsidesCostEstimate:
    """USD cost at the supplied base year, retaining weight and cited factors."""

    total_usd: float
    weight_tonnes: float
    usd_per_tonne: CitedValue
    complexity_factor: CitedValue
    price_base_year: int

    @property
    def citations(self) -> dict[str, Citation]:
        """Return the Citation sidecar keyed by its corresponding cost input."""
        return {
            "usd_per_tonne": self.usd_per_tonne.citation,
            "complexity_factor": self.complexity_factor.citation,
        }


def _positive_number(name: str, value: float) -> float:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a positive finite number")
    try:
        number = float(value)
    except (ValueError, OverflowError) as exc:
        raise ValueError(f"{name} must be a positive finite number") from exc
    if not isfinite(number) or number <= 0:
        raise ValueError(f"{name} must be a positive finite number")
    return number


def estimate_topsides_cost(
    *,
    weight_tonnes: float,
    usd_per_tonne: CitedValue,
    complexity_factor: CitedValue,
    price_base_year: int,
) -> TopsidesCostEstimate:
    """Calculate weight × unit rate × complexity, with a fail-closed sidecar.

    Weight, rate and complexity shall be positive and finite. Rate units shall
    be ``USD/tonne`` and complexity units shall be ``1`` or ``dimensionless``.
    Both cited inputs shall reference resolvable wiki pages using the existing
    citation resolver.
    Missing citations or unresolved/mismatched metadata raise
    ``CitationResolutionError``; invalid numbers or units raise ``ValueError``.

    Weight is dry topsides mass excluding operating fluids; the supplied rate
    shall use the same dry basis. Basis correspondence is established by review,
    not checked by this method. The price base year is a required integer within
    Python's calendar representation limits. It is a caller assertion, not
    checked against the rate citation. Cost boundaries require basis review.
    No currency conversion, escalation, allowances or complete facility CAPEX
    are calculated. Existing aggregate estimators and DCF adapters are separate.
    """
    if (
        isinstance(price_base_year, bool)
        or not isinstance(price_base_year, Integral)
        or not MINYEAR <= price_base_year <= MAXYEAR
    ):
        raise ValueError(
            f"price_base_year must be an integer calendar year from {MINYEAR} to {MAXYEAR}"
        )
    weight = _positive_number("weight_tonnes", weight_tonnes)
    values = {}
    for name, cited, units in (
        ("usd_per_tonne", usd_per_tonne, ("USD/tonne",)),
        ("complexity_factor", complexity_factor, ("1", "dimensionless")),
    ):
        if not isinstance(cited, CitedValue) or not isinstance(cited.citation, Citation):
            raise CitationResolutionError(
                code_id=name, wiki_path="", reason="citation_missing"
            )
        if cited.units not in units:
            raise ValueError(f"{name} units must be {' or '.join(units)}")
        values[name] = _positive_number(name, cited.value)
        validate_citation(cited.citation)

    total = weight * values["usd_per_tonne"] * values["complexity_factor"]
    if not isfinite(total) or total <= 0:
        raise ValueError("total_usd must be a positive finite number")
    return TopsidesCostEstimate(
        total_usd=total,
        weight_tonnes=weight,
        usd_per_tonne=usd_per_tonne,
        complexity_factor=complexity_factor,
        price_base_year=int(price_base_year),
    )
