# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Quantity-times-rate roll-up, with no embedded engineering or price factors.

Method: OpenStax, Prealgebra 2e (2020), section 5.6, Unit Rate / Unit Price.
Whole-joint procurement uses the least integer that covers the supplied length;
this is a declared procurement convention, not a structural design equation.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
from decimal import Decimal, InvalidOperation, localcontext
from fractions import Fraction
from math import ceil
from pathlib import Path

from digitalmodel.citations import Citation, validate_citation


UNITS = {
    "riser_joints": "joint",
    "coating": "m",
    "buoyancy_modules": "module",
    "ancillaries": "item",
    "installation_spread": "day",
}


def number(value: object, name: str, *, positive: bool = False) -> Decimal:
    """Reject non-finite, negative and boolean input before arithmetic."""
    try:
        result = Decimal(str(value))
    except (InvalidOperation, ValueError):
        raise ValueError(f"{name} must be a decimal number") from None
    if not result.is_finite() or result < 0 or (positive and result == 0):
        raise ValueError(
            f"{name} must be finite and {'positive' if positive else 'nonnegative'}"
        )
    return result


def arithmetic_precision(values: list[Decimal]) -> int:
    """Retain all supplied decimal digits through products and their sum."""
    return sum(
        len(value.as_tuple().digits) + abs(value.as_tuple().exponent)
        for value in values
    ) + len(values)


@dataclass(frozen=True)
class Configuration:
    """Installed SLWR quantities in metres, counts and days; no inferred defaults.

    The citation identifies the supplied quantity schedule, including joint length.
    A distributed buoyant length alone does not establish a module count.
    """

    riser_length_m: object
    joint_length_m: object
    coated_length_m: object
    buoyancy_module_count: object
    ancillary_count: object
    installation_days: object
    citation: Citation


@dataclass(frozen=True)
class Rate:
    """Currency per named unit, with a citation to the external price basis."""

    component: str
    unit: str
    value: object
    citation: Citation


@dataclass(frozen=True)
class CostBasis:
    """One currency and revision; rate values are never bundled by the engine."""

    currency: str
    revision: str
    data_kind: str
    rates: tuple[Rate, ...]


def _quantities(configuration: Configuration) -> dict[str, Decimal]:
    length = number(configuration.riser_length_m, "riser_length_m", positive=True)
    joint = number(configuration.joint_length_m, "joint_length_m", positive=True)
    coating = number(configuration.coated_length_m, "coated_length_m")
    if coating > length:
        raise ValueError("coated_length_m exceeds installed riser length")
    values = {
        "riser_joints": Decimal(ceil(Fraction(length) / Fraction(joint))),
        "coating": coating,
        "buoyancy_modules": number(
            configuration.buoyancy_module_count, "buoyancy_module_count"
        ),
        "ancillaries": number(configuration.ancillary_count, "ancillary_count"),
        "installation_spread": number(
            configuration.installation_days, "installation_days"
        ),
    }
    for component in ("buoyancy_modules", "ancillaries"):
        if values[component] != values[component].to_integral_value():
            raise ValueError(f"{component} must be a whole count")
    return values


def _validate_basis(basis: CostBasis) -> dict[str, Rate]:
    for field in ("currency", "revision"):
        if (
            not isinstance(getattr(basis, field), str)
            or not getattr(basis, field).strip()
        ):
            raise ValueError(f"{field} is required")
    if basis.data_kind not in ("synthetic", "private"):
        raise ValueError("data_kind must be synthetic or private")
    rates = {rate.component: rate for rate in basis.rates}
    if len(rates) != len(basis.rates) or set(rates) != set(UNITS):
        raise ValueError("Exactly one rate for each BOM component is required")
    for component, rate in rates.items():
        if rate.unit != UNITS[component]:
            raise ValueError(f"Unit mismatch for {component}")
        number(rate.value, f"{component} rate")
    return rates


def _citation(citation: Citation, root: Path | None) -> dict:
    if not isinstance(citation, Citation):
        raise ValueError("A Citation sidecar is required for every input basis")
    validate_citation(citation, repo_root=root)
    return asdict(citation)


def cost_bom(
    configuration: Configuration,
    basis: CostBasis,
    *,
    citation_root: Path | None = None,
) -> dict:
    """Return a JSON-ready breakdown and separate citation sidecar.

    All citations resolve at every calculation, including zero-quantity lines.
    The output contains rates and quantities and inherits the basis's privacy;
    private results shall not be published in the public repository.
    """
    rates = _validate_basis(basis)
    sidecar = [
        {
            "applies_to": "configuration",
            **_citation(configuration.citation, citation_root),
        }
    ]
    sidecar.extend(
        {"applies_to": component, **_citation(rates[component].citation, citation_root)}
        for component in UNITS
    )
    quantities = _quantities(configuration)
    decimals = list(quantities.values()) + [
        number(rate.value, rate.component) for rate in rates.values()
    ]
    with localcontext() as context:
        context.prec = arithmetic_precision(decimals)
        rows = []
        total = Decimal(0)
        for component, quantity in quantities.items():
            rate = number(rates[component].value, component)
            subtotal = quantity * rate
            total += subtotal
            rows.append(
                {
                    "component": component,
                    "quantity": str(quantity),
                    "unit": UNITS[component],
                    "unit_rate": str(rate),
                    "subtotal": str(subtotal),
                }
            )
    return {
        "currency": basis.currency,
        "basis_revision": basis.revision,
        "data_kind": basis.data_kind,
        "breakdown": rows,
        "total": str(total),
        "citations": sidecar,
        "limitations": "Trade-study costing only. Fatigue, structural adequacy, escalation, "
        "tax, contingency and currency conversion are NOT_EVALUATED.",
    }
