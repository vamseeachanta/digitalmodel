"""Fail-closed readiness screen for API 579 Part 9 Level 1 crack-like flaws.

API 579-1/ASME FFS-1 (2021) public contents identifies applicability in 9.2.2,
data requirements in 9.3, Level 1 in 9.4.2 and Level 2 in 9.4.3. It does not
provide the assumptions or figure points needed to qualify screening curves.
No Level 1 acceptance curve is supplied by this module.

``quarter_thickness`` and ``full_thickness`` name the requested reference
curves, not the measured flaw depth or a supported acceptance limit. Values
are in mm, MPa, MPa*sqrt(m) and degrees Celsius as named. The intake fields
are a software readiness checklist, not a complete standards applicability
assessment. No defaults are inferred for missing engineering data.
"""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass
from typing import Literal

Geometry = Literal["plate", "cylinder", "sphere"]
ReferenceFlaw = Literal["quarter_thickness", "full_thickness"]

# Section locators only, read from the publisher's 2021 public contents.
# No licensed numeric constants, equations or figure points are reproduced.
METHOD_REFERENCE = "API 579-1/ASME FFS-1 (2021), Part 9, sections 9.2.2, 9.3, 9.4.2"
SOURCE_URL = (
    "https://www.asme.org/getmedia/5726e673-eb65-456e-913d-3d0dce0359ff/"
    "asme-ffs-1_2021_toc.pdf"
)
MISSING_EVIDENCE = (
    "level1_applicability_and_assumptions",
    "qualified_level2_regeneration_basis",
    "geometry_specific_stress_intensity_and_reference_stress",
    "validated_reference_flaw_curves",
    "documented_curve_validation_tolerance",
)


@dataclass(frozen=True)
class Part9Level1Case:
    """Explicit screening request; omitted data remains missing, never zero."""

    geometry: Geometry
    reference_flaw: ReferenceFlaw
    thickness_mm: float | None = None
    flaw_depth_mm: float | None = None
    flaw_length_mm: float | None = None  # total length, not half-length
    inside_radius_mm: float | None = None
    membrane_stress_mpa: float | None = None
    bending_stress_mpa: float | None = None
    assessment_temperature_c: float | None = None
    yield_strength_mpa: float | None = None
    ultimate_strength_mpa: float | None = None
    fracture_toughness_mpa_sqrt_m: float | None = None


@dataclass(frozen=True)
class Part9Level1Result:
    """Readiness record only; a numeric acceptance result is unavailable."""

    case: Part9Level1Case
    missing_inputs: tuple[str, ...]

    def to_dict(self) -> dict:
        """Strict-JSON payload: unknown quantities use null, not NaN or zero."""
        return {
            "status": "NOT_EVALUATED",
            "acceptable": None,
            "allowable_flaw_length_mm": None,
            "inputs": asdict(self.case),
            "missing_inputs": list(self.missing_inputs),
            "missing_evidence": list(MISSING_EVIDENCE),
            "method_reference": METHOD_REFERENCE,
            "source": {
                "url": SOURCE_URL,
                "edition": "2021",
                "content_scope": "table of contents; section locators only",
            },
        }


def screen_part9_level1(case: Part9Level1Case) -> Part9Level1Result:
    """Record readiness without substituting the Level 2 FAD for Level 1."""
    _validate_case(case)
    missing = tuple(
        name for name, value in asdict(case).items()
        if value is None and (name != "inside_radius_mm" or case.geometry != "plate")
    )
    return Part9Level1Result(case=case, missing_inputs=missing)


def _validate_case(case: Part9Level1Case) -> None:
    """Software domain checks only; standards applicability is not established."""
    if case.geometry not in ("plate", "cylinder", "sphere"):
        raise ValueError("geometry must be plate, cylinder or sphere")
    if case.reference_flaw not in ("quarter_thickness", "full_thickness"):
        raise ValueError("reference_flaw must be quarter_thickness or full_thickness")
    signed_fields = {
        "membrane_stress_mpa", "bending_stress_mpa", "assessment_temperature_c",
    }
    for name, value in asdict(case).items():
        if name in ("geometry", "reference_flaw") or value is None:
            continue
        if (
            isinstance(value, bool) or not isinstance(value, (int, float))
            or not math.isfinite(value)
        ):
            raise ValueError(f"{name} must be a finite number")
        # Positivity is a software domain invariant, not a code acceptance limit.
        if name not in signed_fields and value <= 0:
            raise ValueError(f"{name} must be positive")
    if case.flaw_depth_mm is not None and case.thickness_mm is not None:
        if case.flaw_depth_mm > case.thickness_mm:
            raise ValueError("flaw_depth_mm must not exceed thickness_mm")
    if case.yield_strength_mpa is not None and case.ultimate_strength_mpa is not None:
        if case.ultimate_strength_mpa < case.yield_strength_mpa:
            raise ValueError("ultimate_strength_mpa must not be below yield_strength_mpa")
