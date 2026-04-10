# ABOUTME: Hull form parametric design — form coefficients and relationships
# ABOUTME: Block, prismatic, midship, waterplane coefficients + Series 60
"""
Hull form parametric design calculations.

Covers form coefficients (Cb, Cp, Cm, Cwp), their interrelationships,
and Series 60 hull form regression data.

References:
- USNA EN400 Chapter 2 — Hull form geometry
- PNA Vol I — Hull form coefficients
- Todd, Series 60 — Methodical Experiments (DTMB Report 1712)
"""

import math
from dataclasses import dataclass
from typing import Any, Mapping, Optional

# Seawater properties (15°C)
RHO_SW = 1025.0  # kg/m³


@dataclass(frozen=True)
class RigHullEstimate:
    """Estimated drilling-rig hull dimensions in metric units."""

    loa_m: Optional[float]
    beam_m: Optional[float]
    draft_m: Optional[float]
    displacement_tonnes: Optional[float]
    confidence: str


@dataclass(frozen=True)
class RigHullValidation:
    """Hull-form validation result for a drilling rig record."""

    rig_name: str
    rig_type: str
    hull_form_type: str
    loa_m: float
    beam_m: float
    draft_m: float
    aspect_ratio: float
    geometry_family: str
    matched_family: str
    matched_hull_id: str
    similarity_score: float
    dimension_confidence: str
    is_valid: bool
    reason: str


_RIG_TYPE_TO_HULL_FORM: dict[str, str] = {
    "drillship": "drillship",
    "semi_submersible": "semi_sub",
}

_RIG_HULL_DEFAULTS: dict[str, RigHullEstimate] = {
    "drillship": RigHullEstimate(
        loa_m=228.0,
        beam_m=42.0,
        draft_m=12.0,
        displacement_tonnes=96000.0,
        confidence="generic",
    ),
    "semi_sub": RigHullEstimate(
        loa_m=104.0,
        beam_m=78.0,
        draft_m=21.0,
        displacement_tonnes=45000.0,
        confidence="generic",
    ),
}

_DRILLSHIP_BY_DEPTH: list[tuple[float, RigHullEstimate]] = [
    (5000.0, RigHullEstimate(190.0, 36.0, 10.0, 55000.0, "estimated")),
    (8000.0, RigHullEstimate(228.0, 42.0, 12.0, 96000.0, "estimated")),
    (12000.0, RigHullEstimate(238.0, 42.0, 12.0, 105000.0, "estimated")),
]

_SEMI_SUB_BY_DEPTH: list[tuple[float, RigHullEstimate]] = [
    (3000.0, RigHullEstimate(85.0, 67.0, 18.0, 30000.0, "estimated")),
    (7500.0, RigHullEstimate(104.0, 78.0, 21.0, 45000.0, "estimated")),
    (12000.0, RigHullEstimate(120.0, 90.0, 24.0, 55000.0, "estimated")),
]

_SEMI_SUB_MAX_ASPECT_RATIO = 2.5
_SHIP_MIN_ASPECT_RATIO = 3.0


def rig_type_to_hull_form(rig_type: Optional[str]) -> Optional[str]:
    """Map worldenergydata drilling rig types to hydrodynamic hull forms."""
    if not rig_type:
        return None
    normalized = str(rig_type).strip().lower()
    if not normalized:
        return None
    return _RIG_TYPE_TO_HULL_FORM.get(normalized)


def estimate_rig_hull_dimensions(
    rig_type: Optional[str],
    *,
    water_depth_ft: Optional[float] = None,
) -> Optional[RigHullEstimate]:
    """Estimate principal dimensions for drillships and semi-subs."""
    hull_form = rig_type_to_hull_form(rig_type)
    if hull_form is None:
        return None

    if water_depth_ft is not None and water_depth_ft > 0:
        brackets = _DRILLSHIP_BY_DEPTH if hull_form == "drillship" else _SEMI_SUB_BY_DEPTH
        for max_depth, estimate in brackets:
            if water_depth_ft <= max_depth:
                return estimate
        return brackets[-1][1]

    return _RIG_HULL_DEFAULTS[hull_form]


def classify_rig_hull_geometry(loa_m: float, beam_m: float) -> str:
    """Classify a hull family from measured LOA/beam geometry."""
    if loa_m <= 0 or beam_m <= 0:
        raise ValueError("loa_m and beam_m must be positive")
    aspect_ratio = loa_m / beam_m
    if aspect_ratio <= _SEMI_SUB_MAX_ASPECT_RATIO:
        return "semi_sub"
    if aspect_ratio >= _SHIP_MIN_ASPECT_RATIO:
        return "ship_shape"
    return "ambiguous"


def validate_drilling_rig_hull_form(
    record: Mapping[str, Any],
) -> Optional[RigHullValidation]:
    """Validate drillship and semi-sub hull geometry against expected hull form.

    Only supported drilling-rig types with LOA and beam geometry are evaluated.
    Missing draft is estimated from type and water-depth bracket so the result can
    also be compared to the built-in hull-library candidates.
    """
    rig_name = str(record.get("RIG_NAME") or "").strip()
    rig_type = str(record.get("RIG_TYPE") or "").strip().lower()
    hull_form_type = rig_type_to_hull_form(rig_type)
    if not rig_name or hull_form_type is None:
        return None

    loa_m = _coerce_positive_number(record.get("LOA_M"))
    beam_m = _coerce_positive_number(record.get("BEAM_M"))
    if loa_m is None or beam_m is None:
        return None

    water_depth_ft = _coerce_positive_number(
        record.get("WATER_DEPTH_RATING_FT", record.get("MAX_WATER_DEPTH_FT"))
    )
    estimate = estimate_rig_hull_dimensions(rig_type, water_depth_ft=water_depth_ft)
    draft_m = _coerce_positive_number(record.get("DRAFT_M"))
    if draft_m is None and estimate is not None:
        draft_m = estimate.draft_m
        dimension_confidence = estimate.confidence
    else:
        dimension_confidence = "measured"
    if draft_m is None:
        return None

    aspect_ratio = loa_m / beam_m
    geometry_family = classify_rig_hull_geometry(loa_m, beam_m)

    from digitalmodel.hydrodynamics.hull_library.lookup import HullLookup, HullLookupTarget

    match = HullLookup().get_hull_form(
        HullLookupTarget(loa_m=loa_m, beam_m=beam_m, draft_m=draft_m)
    )
    matched_family = _matched_hull_family(match.hull_id)
    expected_family = "semi_sub" if hull_form_type == "semi_sub" else "ship_shape"

    reasons: list[str] = []
    if geometry_family != expected_family:
        reasons.append(
            f"geometry family {geometry_family} does not match expected {expected_family}"
        )
    if matched_family != expected_family:
        reasons.append(
            f"nearest hull {match.hull_id} maps to {matched_family}, expected {expected_family}"
        )

    return RigHullValidation(
        rig_name=rig_name,
        rig_type=rig_type,
        hull_form_type=hull_form_type,
        loa_m=loa_m,
        beam_m=beam_m,
        draft_m=draft_m,
        aspect_ratio=aspect_ratio,
        geometry_family=geometry_family,
        matched_family=matched_family,
        matched_hull_id=match.hull_id,
        similarity_score=match.similarity_score,
        dimension_confidence=dimension_confidence,
        is_valid=not reasons,
        reason="; ".join(reasons) if reasons else "validated",
    )


def validate_drilling_rig_fleet(
    records: list[Mapping[str, Any]],
) -> list[RigHullValidation]:
    """Validate the drillship and semi-sub fleet subset with usable geometry."""
    results: list[RigHullValidation] = []
    for record in records:
        validation = validate_drilling_rig_hull_form(record)
        if validation is not None:
            results.append(validation)
    return results


def summarize_drilling_rig_hull_validation(
    results: list[RigHullValidation],
) -> dict[str, Any]:
    """Summarize drilling-rig hull validation results."""
    valid_count = sum(1 for result in results if result.is_valid)
    invalid = [result.rig_name for result in results if not result.is_valid]
    by_type: dict[str, int] = {}
    for result in results:
        by_type[result.rig_type] = by_type.get(result.rig_type, 0) + 1
    return {
        "processed": len(results),
        "valid": valid_count,
        "invalid": len(results) - valid_count,
        "invalid_rigs": invalid,
        "by_type": by_type,
    }


def _matched_hull_family(hull_id: str) -> str:
    if hull_id.startswith("SEMI-"):
        return "semi_sub"
    return "ship_shape"


def _coerce_positive_number(value: Any) -> Optional[float]:
    if isinstance(value, bool) or value is None:
        return None
    if isinstance(value, (int, float)):
        result = float(value)
        return result if result > 0 else None
    if isinstance(value, str):
        stripped = value.strip()
        if not stripped:
            return None
        try:
            result = float(stripped)
        except ValueError:
            return None
        return result if result > 0 else None
    return None


def block_coefficient(
    displacement_m3: float,
    lwl_m: float,
    beam_m: float,
    draft_m: float,
) -> float:
    """Block coefficient Cb = V / (L * B * T).

    Measures fullness of underwater hull relative to bounding box.
    Typical: 0.35 (destroyer) to 0.85 (tanker).
    """
    box_vol = lwl_m * beam_m * draft_m
    if box_vol <= 0:
        raise ValueError("Dimensions must be positive")
    return displacement_m3 / box_vol


def prismatic_coefficient(cb: float, cm: float) -> float:
    """Prismatic coefficient Cp = Cb / Cm.

    Measures longitudinal distribution of displacement.
    """
    if cm <= 0:
        raise ValueError("Midship coefficient must be positive")
    return cb / cm


def midship_coefficient(
    midship_area_m2: float,
    beam_m: float,
    draft_m: float,
) -> float:
    """Midship section coefficient Cm = Am / (B * T)."""
    if beam_m <= 0 or draft_m <= 0:
        raise ValueError("Beam and draft must be positive")
    return midship_area_m2 / (beam_m * draft_m)


def waterplane_coefficient(
    waterplane_area_m2: float,
    lwl_m: float,
    beam_m: float,
) -> float:
    """Waterplane area coefficient Cwp = Awp / (L * B)."""
    if lwl_m <= 0 or beam_m <= 0:
        raise ValueError("Length and beam must be positive")
    return waterplane_area_m2 / (lwl_m * beam_m)


def displacement_from_cb(
    cb: float,
    lwl_m: float,
    beam_m: float,
    draft_m: float,
    rho: float = RHO_SW,
) -> float:
    """Displacement in tonnes from block coefficient.

    Delta = Cb * L * B * T * rho / 1000
    """
    return cb * lwl_m * beam_m * draft_m * rho / 1000.0


def froude_number(speed_ms: float, lwl_m: float) -> float:
    """Froude number Fn = V / sqrt(g * L)."""
    return speed_ms / math.sqrt(9.81 * lwl_m)


def wetted_surface_denny_mumford(
    displacement_m3: float,
    lwl_m: float,
) -> float:
    """Wetted surface approximation (Denny-Mumford formula).

    S = 1.7 * L * T + V/T  (simplified)
    More practical: S ≈ 2.6 * sqrt(displacement * L)
    """
    return 2.6 * math.sqrt(displacement_m3 * lwl_m)


def series_60_cr(cb: float, fn: float) -> float:
    """Series 60 residuary resistance coefficient (simplified).

    Approximate regression for Series 60 parent forms.
    Valid for 0.60 ≤ Cb ≤ 0.80 and 0.15 ≤ Fn ≤ 0.32.

    Returns Cr × 1000 (dimensionless × 10³).
    """
    if not 0.55 <= cb <= 0.85:
        raise ValueError(f"Cb={cb} outside Series 60 range (0.55-0.85)")
    if not 0.10 <= fn <= 0.35:
        raise ValueError(f"Fn={fn} outside valid range (0.10-0.35)")

    # Simplified quadratic regression from Series 60 data
    # Cr increases with both Cb and Fn
    cr = (0.5 + 3.5 * (cb - 0.60) + 8.0 * fn**2
          + 12.0 * (cb - 0.60) * fn**2)
    return max(cr, 0.0)


def lcb_from_cb(cb: float) -> float:
    """Approximate LCB position (% LWL fwd of midship) from Cb.

    Empirical: LCB ≈ -13.5 + 19.4 * Cb (Harvald approximation).
    Negative = aft of midship.
    """
    return -13.5 + 19.4 * cb
