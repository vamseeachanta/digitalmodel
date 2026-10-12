# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Normalized axial residual capacity from Cho (1987), Volume I.

The calculation implements equations 6.13 to 6.17 for zero hydrostatic
pressure. It does not establish engineering acceptance or an API RP 2SIM
assessment level. All inputs and numerical outputs are dimensionless.
"""

from __future__ import annotations

from dataclasses import dataclass, fields
import math
from typing import Literal


@dataclass(frozen=True)
class SourceReference:
    """Public source designation and the locator supporting a calculation."""

    designation: str
    year: int
    locator: str
    url: str


SOURCE_URL = "https://theses.gla.ac.uk/77600/1/10997383.pdf"
CITATIONS = tuple(
    SourceReference(
        "Cho, Design Approximations for Offshore Tubulars Against Collisions, Volume I",
        1987, locator, SOURCE_URL,
    )
    for locator in (
        "Notation, pages 3, 7, 8",
        "Section 6.3.1, page 193",
        "Equation 6.13, page 194",
        "Equation 6.14, page 194",
        "Equation 6.15, page 194",
        "Equation 6.16, page 194 (zero pressure)",
        "Equation 6.17, page 194",
        "Section 6.4.2.1, page 197",
    )
)

# Study-grid extrema, not independently validated continuous ranges:
# Cho (1987), Volume I, section 6.3.1, page 193.
STUDY_ENVELOPE = {
    "midwall_diameter_to_thickness": (20.0, 60.0),
    "reduced_slenderness": (0.25, 1.5),
    "dent_depth_ratio": (0.0, 0.15),
    "neutral_axis_bow_ratio": (0.0005, 0.02),
}
STUDY_ELASTIC_TO_YIELD_RATIO = 600.0  # Cho (1987), section 6.3.1, page 193.


@dataclass(frozen=True)
class DentedAxialInput:
    """Dimensionless inputs, Cho (1987) notation and section 6.4.2.1.

    Diameter is measured at wall mid-thickness, not at the outer surface.
    Reduced slenderness is sqrt(yield stress / elastic column buckling stress).
    Dent depth is divided by mid-thickness diameter. Bow at the plastic
    neutral axis is divided by member length; an opposite-generatrix
    measurement cannot be substituted. Demand is axial stress / yield stress.
    Elastic/yield ratio is the elastic modulus divided by yield stress.
    Conditions require explicit confirmation; no support/load assumption
    is supplied by default. All numeric inputs use consistent stress units.
    """

    midwall_diameter_to_thickness: float
    reduced_slenderness: float
    dent_depth_ratio: float
    neutral_axis_bow_ratio: float
    elastic_to_yield_ratio: float
    axial_demand_ratio: float
    simply_supported: bool
    sharp_midspan_dent: bool
    bow_at_plastic_neutral_axis: bool
    has_applied_bending: bool
    has_hydrostatic_pressure: bool


@dataclass(frozen=True)
class DentedAxialResult:
    """Predicted capacity/yield and demand/capacity, with source sidecar.

    Unsupported inputs return no numerical capacity or utilization. A
    comparison with predicted capacity is not a structural acceptance verdict.
    """

    capacity_ratio: float | None
    utilization: float | None
    verdict: Literal[
        "within_predicted_capacity", "exceeds_predicted_capacity",
        "outside_applicability",
    ]
    applicability_flags: tuple[str, ...]
    citations: tuple[SourceReference, ...] = CITATIONS
    validation_status: str = "hand_calculation_only"
    engineering_acceptance_established: bool = False


def _validate_inputs(data: DentedAxialInput) -> None:
    """Reject undefined quantities before evaluating Cho (1987) equations."""
    if not isinstance(data, DentedAxialInput):
        raise TypeError("data must be DentedAxialInput")
    positive = {
        "midwall_diameter_to_thickness", "reduced_slenderness",
        "elastic_to_yield_ratio",
    }
    for item in fields(data):
        value = getattr(data, item.name)
        if item.name in STUDY_ENVELOPE or item.name in {
            "elastic_to_yield_ratio", "axial_demand_ratio",
        }:
            if isinstance(value, bool) or not isinstance(value, (int, float)):
                raise TypeError(f"{item.name} must be numerical")
            if not math.isfinite(value) or value < 0:
                raise ValueError(f"{item.name} must be finite and nonnegative")
            if item.name in positive and value == 0:
                raise ValueError(f"{item.name} must be positive")
        elif not isinstance(value, bool):
            raise TypeError(f"{item.name} must be a boolean condition")


def _applicability_flags(data: DentedAxialInput) -> tuple[str, ...]:
    """Apply Cho (1987) section 6.3.1 envelope and section 6.4.2.1 scope."""
    flags = [
        name for name, (low, high) in STUDY_ENVELOPE.items()
        if not low <= getattr(data, name) <= high
    ]
    if data.elastic_to_yield_ratio != STUDY_ELASTIC_TO_YIELD_RATIO:
        flags.append("elastic_to_yield_ratio")
    required_conditions = {
        "simply_supported": True,
        "sharp_midspan_dent": True,
        "bow_at_plastic_neutral_axis": True,
        "has_applied_bending": False,
        "has_hydrostatic_pressure": False,
    }
    flags.extend(name for name, expected in required_conditions.items()
                 if getattr(data, name) != expected)
    return tuple(flags)


def _capacity_ratio(data: DentedAxialInput) -> float:
    """Evaluate Cho (1987) equations 6.13 to 6.17, page 194."""
    # Each coefficient/exponent is read from the cited equation.
    overall = 22.2 * (data.neutral_axis_bow_ratio * data.reduced_slenderness)**0.7  # Equation 6.14.
    dent = (1.0 + 1.26 * data.dent_depth_ratio**1.3
            * data.midwall_diameter_to_thickness**0.6)  # Equation 6.15.
    imperfection = overall * dent  # Equations 6.13/6.16: pressure factor exp(0)=1.
    critical = 1.0 / data.reduced_slenderness**2  # Notation, page 8.
    half_sum = (1.0 + (1.0 + imperfection) * critical) / 2.0
    return float(half_sum - math.sqrt(half_sum**2 - critical))  # Equation 6.17.


def screen_dented_axial(data: DentedAxialInput) -> DentedAxialResult:
    """Compare axial demand with Cho (1987) predicted residual capacity.

    Section 6.3.1 study-grid extrema bound the implemented envelope;
    interpolation between grid values is an implementation assumption.
    Section 6.4.2.1 governs dent/support/bow applicability. Applied bending
    and hydrostatic pressure are excluded from this axial subset.
    """
    _validate_inputs(data)
    flags = _applicability_flags(data)
    if flags:
        return DentedAxialResult(None, None, "outside_applicability", flags)
    capacity = _capacity_ratio(data)
    utilization = data.axial_demand_ratio / capacity
    verdict: Literal["within_predicted_capacity", "exceeds_predicted_capacity"]
    verdict = ("within_predicted_capacity" if utilization <= 1.0
               else "exceeds_predicted_capacity")
    return DentedAxialResult(capacity, utilization, verdict, ())
