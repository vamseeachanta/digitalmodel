# ABOUTME: Depth applicability guard for preliminary turning calculations.
"""Depth screening, without a shallow-water correction or validation claim."""

import math

# IMO MSC/Circ.1053 (2002), Annex 2.1.2: trial depth must exceed four drafts.
# https://www.traficom.fi/files/media/regulation/22701-MSC_1-Circ_1053_-_tulkinta_ship_manoeuvrability.pdf
# Applied as an applicability screen for the uncalibrated Nomoto estimator too.
MIN_TURNING_DEPTH_RATIO = 4.0
UNKNOWN_DEPTH_WARNING = (
    "Water depth/draft not supplied; deep-water applicability is unverified. "
    "Supply water_depth_m and draft_m; h/T must exceed 4 "
    "(IMO MSC/Circ.1053, Annex 2.1.2)."
)


def turning_depth_warning(water_depth_m: float | None, draft_m: float | None) -> str | None:
    """Refuse known h/T <= 4; return a warning for unknown depth.

    +inf explicitly declares unrestricted depth. A finite positive draft is
    required when depth is supplied. Passing this screen does not validate
    hydrodynamic coefficients, bank effects or the other IMO trial conditions.
    """
    try:
        if water_depth_m is not None:
            water_depth_m = float(water_depth_m)
        if draft_m is not None:
            draft_m = float(draft_m)
    except (TypeError, ValueError) as exc:
        raise ValueError("water_depth_m and draft_m must be numeric when supplied") from exc
    if draft_m is not None and (not math.isfinite(draft_m) or draft_m <= 0):
        raise ValueError("draft_m must be finite and > 0")
    if water_depth_m is None:
        return UNKNOWN_DEPTH_WARNING
    if draft_m is None:
        raise ValueError("draft_m is required with water_depth_m")
    if math.isnan(water_depth_m) or water_depth_m <= 0:
        raise ValueError("water_depth_m must be positive (or +inf)")
    if water_depth_m / draft_m <= MIN_TURNING_DEPTH_RATIO:
        raise ValueError(
            "Deep-water turning calculation requires h/T > 4 "
            "(IMO MSC/Circ.1053, Annex 2.1.2); shallow-water effects "
            "are not modeled."
        )
    return None
