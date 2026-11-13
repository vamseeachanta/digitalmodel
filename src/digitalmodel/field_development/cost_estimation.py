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
from datetime import MINYEAR, date
from decimal import (
    ROUND_HALF_EVEN,
    Context,
    Decimal,
    DecimalException,
    localcontext,
)
from numbers import Integral, Real
from sys import float_info

from digitalmodel.citations.schema import (
    Citation,
    CitationResolutionError,
    CitedValue,
    validate_citation,
)


@dataclass(frozen=True)
class TopsidesCostEstimate:
    """USD cost at the supplied base year, retaining weight and cited factors."""

    total_usd: Decimal
    weight_tonnes: Decimal
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


def _positive_number(name: str, value: Real | Decimal) -> Decimal:
    if isinstance(value, bool) or not isinstance(value, (Real, Decimal)):
        raise ValueError(f"{name} must be a positive finite number")
    try:
        number = Decimal(str(value))
    except (DecimalException, ValueError, OverflowError) as exc:
        raise ValueError(f"{name} must be a positive finite number") from exc
    if not number.is_finite() or number <= 0:
        raise ValueError(f"{name} must be a positive finite number")
    return number


def estimate_topsides_cost(
    *,
    weight_tonnes: Real | Decimal,
    usd_per_tonne: CitedValue,
    complexity_factor: CitedValue,
    price_base_year: int,
    today: date | None = None,
) -> TopsidesCostEstimate:
    """Calculate weight × unit rate × complexity, with a fail-closed sidecar.

    Weight, rate and complexity shall be positive and finite. Rate units shall
    be ``USD/tonne`` and complexity units shall be ``1`` or ``dimensionless``.
    Both cited inputs shall reference resolvable wiki pages using the existing
    citation resolver.
    Missing citations or unresolved/mismatched metadata raise
    ``CitationResolutionError``; invalid numbers, units or dates raise ``ValueError``.

    Weight is dry topsides mass excluding operating fluids; the supplied rate
    shall use the same dry basis. Basis correspondence is established by review,
    not checked by this method. The price base year is a required integer from
    year 1 through the calculation year (``today``, defaulting to the current
    date). A supplied ``today`` shall be a ``date`` instance or ``None``.
    Year 1 is only Python's calendar lower limit: no historical cost
    validity is implied. The year is a caller assertion, not checked against
    the rate citation. Cost boundaries require basis review.
    No currency conversion, escalation, allowances or complete facility CAPEX
    are calculated. Existing aggregate estimators and DCF adapters are separate.

    Each numeric input is converted once using ``Decimal(str(value))``.
    Nondecimal string forms, such as ``Fraction(3, 2)`` (``3/2``), are rejected.
    The exact product is rounded once to USD cents with half-even rounding, in an
    independent decimal context. The total and retained weight are Decimal;
    cited input records are retained unchanged. Totals outside the finite float
    range or rounding to zero cents are rejected, preserving the upper output
    range of the original method.
    """
    if today is not None and not isinstance(today, date):
        raise ValueError("today must be a date or None")
    current_year = (date.today() if today is None else today).year
    if (
        isinstance(price_base_year, bool)
        or not isinstance(price_base_year, Integral)
        or not MINYEAR <= price_base_year <= current_year
    ):
        raise ValueError(
            "price_base_year must be an integer calendar year "
            f"from {MINYEAR} to {current_year}"
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

    factors = [weight, values["usd_per_tonne"], values["complexity_factor"]]
    # Products need at most the sum of coefficient lengths. Reserve enough
    # digits to quantize any total in the supported range to cents as well.
    precision = max(
        sum(len(factor.as_tuple().digits) for factor in factors),
        float_info.max_10_exp + 4,
    )
    try:
        with localcontext(Context(prec=precision, rounding=ROUND_HALF_EVEN)):
            total = factors[0] * factors[1] * factors[2]
            if not total.is_finite() or total > Decimal(str(float_info.max)):
                raise ValueError("total_usd must be a positive finite number")
            total = total.quantize(Decimal("0.01"))
    except DecimalException as exc:
        raise ValueError("total_usd must be a positive finite number") from exc
    if total <= 0:
        raise ValueError("total_usd must be a positive finite number")
    return TopsidesCostEstimate(
        total_usd=total,
        weight_tonnes=weight,
        usd_per_tonne=usd_per_tonne,
        complexity_factor=complexity_factor,
        price_base_year=int(price_base_year),
    )
