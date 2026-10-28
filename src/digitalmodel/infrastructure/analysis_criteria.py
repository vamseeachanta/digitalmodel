# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Explicit analysis metrics; engineering limits are supplied by the caller.

Absolute error: NIST DLMF 1.2.8 (2026), section 3.1(v), equation 3.1.8,
https://dlmf.nist.gov/3.1#E8. Relative error: equation 3.1.9,
https://dlmf.nist.gov/3.1#E9. No engineering tolerance is prescribed here.
"""

from __future__ import annotations

from dataclasses import dataclass
from decimal import Decimal, InvalidOperation
from fractions import Fraction
import operator as comparisons

_COMPARATORS = {
    "<": comparisons.lt,
    "<=": comparisons.le,
    ">": comparisons.gt,
    ">=": comparisons.ge,
}


def _required_text(value: str | None, name: str) -> None:
    if not isinstance(value, str) or not value.strip() or value != value.strip():
        raise ValueError(f"{name} must be nonempty text without surrounding whitespace")


@dataclass(frozen=True, init=False)
class Quantity:
    """An exact decimal value and an opaque unit token; no unit conversion."""

    value: Decimal
    unit: str

    def __init__(self, value: str | Decimal, unit: str) -> None:
        """Refuse inexact and nonfinite numbers before they enter a criterion."""
        _required_text(unit, "unit")
        if not isinstance(value, (str, Decimal)):
            raise ValueError("value must be an exact decimal string or Decimal")
        try:
            number = Decimal(value)
        except InvalidOperation as exc:
            raise ValueError("Invalid decimal value") from exc
        if not number.is_finite():
            raise ValueError("Nonfinite decimal value")
        object.__setattr__(self, "value", number)
        object.__setattr__(self, "unit", unit)

    def to_record(self) -> dict:
        """Return exact decimal text and the declared unit for JSON storage."""
        return {"value": str(self.value), "unit": self.unit}


@dataclass(frozen=True)
class CriterionResult:
    """A numerical comparison outcome, independent of engineering qualification."""

    status: str
    metric: Fraction | None
    unit: str
    criterion: Criterion
    value: Quantity | None
    reference: Quantity | None
    reason: str | None = None

    def to_record(self) -> dict:
        """Retain provenance and exact ratios without converting to binary float."""
        metric = (
            None
            if self.metric is None
            else {
                "numerator": str(self.metric.numerator),
                "denominator": str(self.metric.denominator),
            }
        )
        return {
            "status": self.status,
            "metric": metric,
            "unit": self.unit,
            "reason": self.reason,
            "criterion": self.criterion.to_record(),
            "value": None if self.value is None else self.value.to_record(),
            "reference": None if self.reference is None else self.reference.to_record(),
        }


@dataclass(frozen=True)
class Criterion:
    """A sourced comparison with an explicit response transformation."""

    operator: str
    comparator: str
    response_unit: str
    limit: Quantity
    source: str
    reference_floor: Quantity | None = None
    floor_source: str | None = None

    def __post_init__(self) -> None:
        if self.operator not in ("signed_value", "absolute_residual", "relative_error"):
            raise ValueError("Unknown operator")
        if not isinstance(self.comparator, str) or self.comparator not in _COMPARATORS:
            raise ValueError("Unknown comparator")
        _required_text(self.response_unit, "response_unit")
        _required_text(self.source, "source")
        if not isinstance(self.limit, Quantity):
            raise ValueError("limit must be a Quantity")
        limit_unit = "1" if self.operator == "relative_error" else self.response_unit
        if self.limit.unit != limit_unit:
            raise ValueError("Limit unit does not match the operator metric")
        if self.operator != "signed_value" and self.limit.value < 0:
            raise ValueError("A residual limit must be nonnegative")
        if self.reference_floor is None:
            if self.floor_source is not None:
                raise ValueError("floor_source requires a reference_floor")
        else:
            if not isinstance(self.reference_floor, Quantity):
                raise ValueError("reference_floor must be a Quantity")
            if self.operator != "relative_error":
                raise ValueError("Only relative_error accepts a reference_floor")
            if self.reference_floor.unit != self.response_unit:
                raise ValueError("Denominator floor must have the response unit")
            if self.reference_floor.value <= 0:
                raise ValueError("Denominator floor must be positive")
            _required_text(self.floor_source, "floor_source")

    def evaluate(
        self,
        value: Quantity | None,
        reference: Quantity | None = None,
    ) -> CriterionResult:
        """Compare a declared metric using exact rational arithmetic."""
        if any(
            item is not None and not isinstance(item, Quantity)
            for item in (value, reference)
        ):
            raise ValueError("Evidence must be a Quantity or None")
        if self.operator == "signed_value" and reference is not None:
            raise ValueError("signed_value does not accept a reference")
        if value is None or (reference is None and self.operator != "signed_value"):
            return self._result(None, value, reference, "Missing evidence")
        if value.unit != self.response_unit or (
            reference is not None and reference.unit != self.response_unit
        ):
            return self._result(None, value, reference, "Unit mismatch")
        metric = Fraction(value.value)
        if reference is not None:
            metric = abs(metric - Fraction(reference.value))
            if self.operator == "relative_error":
                denominator = abs(Fraction(reference.value))
                # Declared denominator policy; not a tolerance from DLMF eq. 3.1.9.
                if self.reference_floor is not None:
                    denominator = max(denominator, Fraction(self.reference_floor.value))
                if denominator == 0:
                    return self._result(
                        None,
                        value,
                        reference,
                        "Zero reference requires an explicit denominator floor",
                    )
                metric /= denominator
        return self._result(metric, value, reference)

    def _result(self, metric, value, reference, reason=None) -> CriterionResult:
        passed = metric is not None and _COMPARATORS[self.comparator](
            metric,
            Fraction(self.limit.value),
        )
        status = "NOT_EVALUATED" if metric is None else "PASS" if passed else "FAIL"
        return CriterionResult(
            status, metric, self.limit.unit, self, value, reference, reason
        )

    def to_record(self) -> dict:
        """Expose the operator, comparator, limit and floor provenance together."""
        return {
            "operator": self.operator,
            "comparator": self.comparator,
            "response_unit": self.response_unit,
            "limit": self.limit.to_record(),
            "source": self.source,
            "reference_floor": None
            if self.reference_floor is None
            else self.reference_floor.to_record(),
            "floor_source": self.floor_source,
        }
