# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Lifecycle cost comparison of two already-sized CP alternatives.

Calculation basis: a cost C at the end of year t has present value
C / (1 + r)**t for the caller's annual effective discount rate r.
Initial capital is paid at time zero and is undiscounted. Zero and one
in this module are mathematical identities or year-index boundaries,
not empirical constants. Positive difference means ICCP has higher NPV cost.

Every annual operating and replacement cost, including an explicit zero,
must be supplied. Values must share a monetary basis consistent with the
discount rate; escalation, taxes, salvage and currency conversion are not
inferred. Currency is a caller-supplied uppercase three-letter code, not a
validated currency registry entry. Provenance records the supplied sizing,
cost and financial assumptions; it does not qualify either design.
"""

from __future__ import annotations

from math import exp, fsum, log, log1p
from typing import Annotated, Literal

from pydantic import BaseModel, ConfigDict, Field, StringConstraints, model_validator

Cost = Annotated[float, Field(strict=True, ge=0, allow_inf_nan=False)]
Year = Annotated[int, Field(strict=True, gt=0)]
BasisText = Annotated[
    str, StringConstraints(strict=True, strip_whitespace=True, min_length=1)
]
Currency = Annotated[
    str,
    StringConstraints(strict=True, min_length=3, max_length=3, pattern=r"^[A-Z]{3}$"),
]


class _ComparisonModel(BaseModel):
    model_config = ConfigDict(
        extra="forbid", frozen=True, revalidate_instances="always"
    )


class AnnualCost(_ComparisonModel):
    """Explicit costs paid at the end of one design-life year."""

    year: Year
    operating_cost: Cost
    replacement_cost: Cost


class AlternativeInput(_ComparisonModel):
    """Already-sized alternative with capital and complete annual costs.

    Provenance must identify the sizing basis and the source or assumptions
    of the capital, operating and replacement costs. A synthetic basis is
    permitted when labelled as such. No price or sizing default is supplied.
    """

    currency: Currency
    initial_cost: Cost
    annual_costs: tuple[AnnualCost, ...]
    provenance: BasisText


class ComparisonInput(_ComparisonModel):
    """Common financial basis and separate ICCP and SACP cost schedules.

    The discount rate is an annual effective fraction, nonnegative with no
    upper cap. The shared life and rate apply to both alternatives. Financial
    provenance must state the monetary basis and the treatment of escalation
    and terminal costs. Rows may arrive in any order; each design-life year
    must appear exactly once in each alternative.
    """

    design_life_years: Year
    discount_rate: Cost
    provenance: BasisText
    iccp: AlternativeInput
    sacp: AlternativeInput

    @model_validator(mode="after")
    def check_common_basis(self) -> ComparisonInput:
        """Reject different currencies and incomplete or duplicate years."""
        if self.iccp.currency != self.sacp.currency:
            raise ValueError("ICCP and SACP currencies must match")
        for label, alternative in (("ICCP", self.iccp), ("SACP", self.sacp)):
            years = sorted(row.year for row in alternative.annual_costs)
            if len(years) != self.design_life_years or any(
                year != expected for expected, year in enumerate(years, start=1)
            ):
                raise ValueError(
                    f"{label} schedule must contain each design-life year exactly once"
                )
        return self


class CategoryTotals(_ComparisonModel):
    """Initial, operating, replacement and combined costs in one currency."""

    initial_cost: Cost
    operating_cost: Cost
    replacement_cost: Cost
    total_cost: Cost


class AlternativeResult(_ComparisonModel):
    """Undiscounted totals and present-value totals for one alternative."""

    undiscounted: CategoryTotals
    npv: CategoryTotals


class ComparisonResult(_ComparisonModel):
    """Cost-only comparison with its complete validated input basis."""

    currency: Currency
    discount_timing: Literal["end_of_year"] = "end_of_year"
    iccp: AlternativeResult
    sacp: AlternativeResult
    npv_difference_iccp_minus_sacp: Annotated[
        float, Field(strict=True, allow_inf_nan=False)
    ]
    input_basis: ComparisonInput


def _totals(initial: float, operating: float, replacement: float) -> CategoryTotals:
    return CategoryTotals(
        initial_cost=initial,
        operating_cost=operating,
        replacement_cost=replacement,
        total_cost=fsum((initial, operating, replacement)),
    )


def _present_value(cost: float, rate: float, year: int) -> float:
    if cost == 0:
        return cost
    try:
        return cost / (1 + rate)**year
    except OverflowError:
        # Same equation in log space when the denominator exceeds float range.
        # This avoids underflowing a reciprocal before scaling by a large cost.
        return exp(log(cost) - year * log1p(rate))


def _evaluate(alternative: AlternativeInput, discount_rate: float) -> AlternativeResult:
    return AlternativeResult(
        undiscounted=_totals(
            alternative.initial_cost,
            fsum(row.operating_cost for row in alternative.annual_costs),
            fsum(row.replacement_cost for row in alternative.annual_costs),
        ),
        npv=_totals(
            alternative.initial_cost,
            fsum(
                _present_value(row.operating_cost, discount_rate, row.year)
                for row in alternative.annual_costs
            ),
            fsum(
                _present_value(row.replacement_cost, discount_rate, row.year)
                for row in alternative.annual_costs
            ),
        ),
    )


def compare_systems(inputs: ComparisonInput) -> ComparisonResult:
    """Compare supplied lifecycle costs without sizing or price inference.

    Args:
        inputs: Validated common basis and complete costs for two alternatives.

    Returns:
        Category totals, end-of-year NPV, ICCP-minus-SACP difference and inputs.

    Raises:
        ValueError: Inputs are invalid or cost totals exceed finite float range.
    """
    basis = ComparisonInput.model_validate(inputs)
    try:
        iccp = _evaluate(basis.iccp, basis.discount_rate)
        sacp = _evaluate(basis.sacp, basis.discount_rate)
    except OverflowError as exc:
        raise ValueError("Cost totals exceed finite float range") from exc
    return ComparisonResult(
        currency=basis.iccp.currency,
        iccp=iccp,
        sacp=sacp,
        npv_difference_iccp_minus_sacp=iccp.npv.total_cost - sacp.npv.total_cost,
        input_basis=basis,
    )
