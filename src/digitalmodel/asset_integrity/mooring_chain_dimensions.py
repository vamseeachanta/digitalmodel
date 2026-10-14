"""Local chain dimensional checks; no breaking-load or fatigue-life verdict.

IACS Recommendation 38, revision 2 (July 2020), sections 1 and 5.1 through
5.4: source and rights metadata accompany results. The perpendicular mean and
equivalent-circular area ratio use arithmetic geometry only; operator limits
and absolute measurement uncertainty are required inputs without defaults.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, fields
from typing import Literal

from digitalmodel.asset_integrity.applicability import Applicability, merge

Status = Literal[
    "WITHIN_LIMITS", "EXCEEDS_LIMITS", "INDETERMINATE", "OUT_OF_SCOPE"
]

@dataclass(frozen=True)
class DiameterReading:
    """Two perpendicular diameters and their shared absolute bound, in mm."""

    location_id: str
    region: str
    diameter_a_mm: float
    diameter_b_mm: float
    uncertainty_mm: float


@dataclass(frozen=True)
class VisualInspection:
    """True means observed; False means inspected negative; None is unknown."""

    crack: bool | None
    loose_stud: bool | None
    missing_stud: bool | None
    geometry_anomaly: bool | None
    pitting: bool | None


@dataclass(frozen=True)
class ChainInspection:
    """Required design reference, operator criteria and inspection evidence."""

    nominal_diameter_mm: float
    application: str
    grade: str
    stud_attachment: str
    min_area_ratio: float
    max_diameter_loss_fraction: float
    operator_limit_reference: str
    readings: tuple[DiameterReading, ...]
    uniform_loss_confirmed: bool
    perpendicular_readings_confirmed: bool
    coverage_confirmed: bool
    visual: VisualInspection


@dataclass(frozen=True)
class DimensionalResult:
    """A local interval comparison; area ratio is an equivalent-circle proxy."""

    location_id: str
    status: Status
    mean_diameter_range_mm: tuple[float, float] | None
    diameter_loss_range_fraction: tuple[float, float] | None
    area_ratio_range: tuple[float, float] | None
    applicability: Applicability


@dataclass(frozen=True)
class ChainDimensionalScreen:
    """Input provenance and local results; no overall FFS verdict is issued."""

    inspection: ChainInspection
    samples: tuple[DimensionalResult, ...]
    applicability: Applicability
    citations: tuple[dict[str, str], ...]
    scope: str
    overall_verdict: None


def _number(value: float, name: str, *, positive: bool) -> None:
    """Validate dimensional scalars, rejecting booleans and nonfinite bounds."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} requires a numerical value")
    try:
        finite = math.isfinite(value)
    except OverflowError as exc:
        raise ValueError(f"{name} is not representable") from exc
    if not finite or value < 0 or (positive and value == 0):
        raise ValueError(f"{name} requires a finite valid bound")


def _validate(inspection: ChainInspection) -> None:
    """Validate values separately from method applicability."""
    if not isinstance(inspection, ChainInspection):
        raise ValueError("ChainInspection required")
    _number(inspection.nominal_diameter_mm, "nominal_diameter_mm", positive=True)
    _number(inspection.min_area_ratio, "min_area_ratio", positive=True)
    _number(inspection.max_diameter_loss_fraction, "diameter loss limit", positive=False)
    # Unity bounds define fractions, not standard discard thresholds.
    if inspection.min_area_ratio > 1 or inspection.max_diameter_loss_fraction >= 1:
        raise ValueError("area ratio in (0, 1] and diameter loss limit in [0, 1) required")
    if (
        not isinstance(inspection.operator_limit_reference, str)
        or not inspection.operator_limit_reference.strip()
    ):
        raise ValueError("operator_limit_reference required")
    for name in ("application", "grade", "stud_attachment"):
        if not isinstance(getattr(inspection, name), str):
            raise ValueError(f"{name} requires a string")
    for name in (
        "uniform_loss_confirmed", "perpendicular_readings_confirmed", "coverage_confirmed"
    ):
        if not isinstance(getattr(inspection, name), bool):
            raise ValueError(f"{name} requires a boolean confirmation")
    if not isinstance(inspection.visual, VisualInspection):
        raise ValueError("VisualInspection required")
    for item in fields(inspection.visual):
        value = getattr(inspection.visual, item.name)
        if value is not None and not isinstance(value, bool):
            raise ValueError(f"{item.name} requires a boolean or None")
    if not isinstance(inspection.readings, tuple) or not inspection.readings:
        raise ValueError("nonempty tuple of readings required")
    seen = set()
    for reading in inspection.readings:
        _validate_reading(reading)
        if reading.location_id in seen:
            raise ValueError("location identifiers must be unique")
        seen.add(reading.location_id)


def _validate_reading(reading: DiameterReading) -> None:
    if not isinstance(reading, DiameterReading):
        raise ValueError("DiameterReading required")
    if not isinstance(reading.location_id, str) or not reading.location_id.strip():
        raise ValueError("nonempty location_id required")
    _number(reading.diameter_a_mm, "diameter_a_mm", positive=True)
    _number(reading.diameter_b_mm, "diameter_b_mm", positive=True)
    _number(reading.uncertainty_mm, "uncertainty_mm", positive=False)
    if reading.uncertainty_mm >= min(reading.diameter_a_mm, reading.diameter_b_mm):
        raise ValueError("uncertainty must leave both diameters positive")
    if not all(
        math.isfinite(d + reading.uncertainty_mm)
        for d in (reading.diameter_a_mm, reading.diameter_b_mm)
    ):
        raise ValueError("diameter uncertainty bounds must be finite")


def _scope(inspection: ChainInspection) -> Applicability:
    """IACS sections 1/5.1 define the supported subset; defects require review."""
    flags, notes = [], []
    supported = {
        ("R3", "mechanical_and_other_end_fillet_weld"),
        ("R4", "mechanical_both_ends"),
    }
    if (
        inspection.application != "mobile_offshore_drilling_unit"
        or (inspection.grade, inspection.stud_attachment) not in supported
    ):
        flags.append("UNSUPPORTED_CHAIN_SCOPE")
        notes.append("Application or attachment is outside the implemented IACS subset.")
    for name in (
        "uniform_loss_confirmed", "perpendicular_readings_confirmed", "coverage_confirmed"
    ):
        if not getattr(inspection, name):
            flags.append(f"{name.removesuffix('_confirmed').upper()}_NOT_CONFIRMED")
            notes.append(f"{name} is not established; local clearance is withheld.")
    for item in fields(inspection.visual):
        if getattr(inspection.visual, item.name) is not False:
            flags.append(f"VISUAL_{item.name.upper()}")
            notes.append(f"{item.name} is present or uninspected; engineering review required.")
    return Applicability(ok=not flags, flags=flags, notes=notes)


def _sample(
    reading: DiameterReading, inspection: ChainInspection, scope: Applicability
) -> DimensionalResult:
    """Propagate absolute bounds; compare entire intervals with operator limits."""
    # Arithmetic mean divisor: two measured axes (IACS section 5.2).
    mean = reading.diameter_a_mm / 2 + reading.diameter_b_mm / 2
    lower, upper = mean - reading.uncertainty_mm, mean + reading.uncertainty_mm
    nominal = inspection.nominal_diameter_mm
    local = Applicability()
    if reading.region not in ("bend", "fairlead_shoulder"):
        local = Applicability(
            False, ["UNSUPPORTED_LOCATION"],
            ["Individual bend or fairlead shoulder diameters required; "
             "interlink double diameters are not inferred."],
        )
    elif upper > nominal:
        local = Applicability(
            False, ["ABOVE_DESIGN_REFERENCE"],
            ["Diameter interval exceeds nominal reference; "
             "the loss-only proxy is out of scope."],
        )
    validity = merge(scope, local)
    if not validity.ok:
        return DimensionalResult(
            reading.location_id, "OUT_OF_SCOPE", None, None, None, validity
        )
    # (D-d)/D avoids cancellation at small losses; both bounds are monotone.
    loss = ((nominal - upper) / nominal, (nominal - lower) / nominal)
    # Circular area ratio: d*d / D*D; normalization prevents large-area overflow.
    area = ((lower / nominal) ** 2, (upper / nominal) ** 2)
    if area[0] <= 0:
        raise ValueError("area ratio is outside representable positive precision")
    status = _interval_status(loss, (lower, upper), inspection)
    return DimensionalResult(reading.location_id, status, (lower, upper), loss, area, validity)


def _interval_status(
    loss: tuple[float, float], diameter: tuple[float, float], inspection: ChainInspection
) -> Status:
    """An interval must establish both comparisons before local clearance."""
    # Compare products, without squaring a rounded quotient. A common exact
    # power-of-two scale puts nominal in [1, 2), avoiding large-area overflow
    # without scaling the limit product below its positive input precision.
    exponent = math.frexp(inspection.nominal_diameter_mm)[1] - 1
    nominal = math.ldexp(inspection.nominal_diameter_mm, -exponent)
    lower, upper = (math.ldexp(bound, -exponent) for bound in diameter)
    area_limit = inspection.min_area_ratio * nominal * nominal
    status: Status
    if (
        loss[1] <= inspection.max_diameter_loss_fraction
        and lower * lower >= area_limit
    ):
        status = "WITHIN_LIMITS"
    elif (
        loss[0] > inspection.max_diameter_loss_fraction
        or upper * upper < area_limit
    ):
        status = "EXCEEDS_LIMITS"
    else:
        status = "INDETERMINATE"
    return status


def screen_dimensions(inspection: ChainInspection) -> ChainDimensionalScreen:
    """Compare inspected-location dimensions; no component-wide acceptance.

    Source: IACS Recommendation 38 sections 1, 2.2.2 and 5.1 through 5.4.
    An operator limit reference is required. No normative numerical threshold
    is embedded. Interlink, pitting, geometry tolerances, residual breaking
    strength and fatigue-life methods remain gaps in the adjacent method note.
    """
    _validate(inspection)
    scope = _scope(inspection)
    samples = tuple(
        _sample(reading, inspection, scope) for reading in inspection.readings
    )
    citation = {
        "title": "Guidelines for the Survey of Offshore Mooring Chain Cable in Use",
        "publisher": "International Association of Classification Societies",
        "document": "Recommendation 38",
        "revision": "Revision 2, July 2020",
        "locator": "Sections 1, 2.2.2, 5.1 through 5.4; pages 1 through 5",
        "url": "https://iacs.s3.af-south-1.amazonaws.com/wp-content/uploads/2022/05/20093306/rec38rev2.pdf",
        "opened": "2026-10-12",
        "reuse_terms": "No reuse licence stated in inspected PDF; citation only",
    }
    regulator_citation = {
        "title": "Study on Mooring System Integrity Management for Floating Structures",
        "publisher": "ABS Consulting for the Bureau of Safety and Environmental Enforcement",
        "document": "final report",
        "revision": "August 4, 2015",
        "locator": "Table 7, page 40",
        "url": "https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program//730-aa.pdf",
        "opened": "2026-10-12",
        "reuse_terms": "No reuse licence is stated in the inspected passages; citation only.",
    }
    return ChainDimensionalScreen(
        inspection, samples, merge(*(s.applicability for s in samples)),
        (citation, regulator_citation), "inspected_locations_only", None,
    )
