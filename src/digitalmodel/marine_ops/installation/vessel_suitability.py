"""
ABOUTME: Reusable, confidence-weighted vessel suitability ranking for a lift.

Promotes the confidence-weighted capability scoring from the demo into a library
API the GTM suitability work (#591/#592) and chat workflows can call directly.
For a lift requirement it ranks the installation fleet (worldenergydata source of
truth + curated, via `vessel_db.installation_vessels`) by capability margin
weighted by data confidence, and flags whether each ranking is *defensible*.

References:
    DNV-RP-H103 (2011) -- Modelling and Analysis of Marine Operations
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional

from digitalmodel.marine_ops.vessel_db.confidence import (
    ConfidenceTier,
    capability_score,
)
from digitalmodel.marine_ops.vessel_db.loader import (
    installation_vessels,
    normalize_vessel_name,
)

# Data-source -> base confidence tier for the crane SWL value.
_SOURCE_TIER = {
    "worldenergydata": ConfidenceTier.CITED,  # real IMO + published reach
    "curated": ConfidenceTier.BROCHURE,  # operator brochure / web research
}


@dataclass
class LiftRequirement:
    """What a lift needs of a vessel."""

    weight_te: float
    radius_m: float
    daf: float = 1.30
    min_dp_class: Optional[int] = None  # e.g. 2 or 3; None = no requirement
    deck_area_m2: Optional[float] = None  # required free deck area


@dataclass
class SuitabilityResult:
    vessel: str
    score: float  # 0..100, confidence-weighted
    defensible: bool
    confidence: ConfidenceTier  # weakest-link tier
    swl_at_radius_te: float
    margin: float  # SWL@radius / (weight * daf-free) static
    limiting_factors: list[str] = field(default_factory=list)
    source: str = ""


def _dp_int(dp_class) -> Optional[int]:
    """Parse a DP class like 'DP3' / 'Lloyd's ... Class 3' -> 3."""
    if dp_class is None:
        return None
    import re

    m = re.search(r"(?:DP[^0-9]*|class[^0-9]*|\b)([0-3])\b", str(dp_class), re.I)
    return int(m.group(1)) if m else None


def assess_vessel(name: str, info: dict, req: LiftRequirement) -> SuitabilityResult:
    """Score one installation-vessel entry (from installation_vessels) for a lift."""
    curve = info["crane_curve"]
    swl = curve.capacity_at_radius(req.radius_m)
    static_margin = swl / req.weight_te if req.weight_te > 0 else 0.0
    dyn_margin = swl / (req.weight_te * req.daf) if req.weight_te > 0 else 0.0

    base_tier = _SOURCE_TIER.get(info.get("source", ""), ConfidenceTier.GENERIC)
    # Headline-only SWL (no published radius curve) is weaker evidence.
    headline_only = len(curve.radii_m) <= 1 or float(curve.radii_m.max()) == 0.0
    crane_tier = (
        ConfidenceTier.ESTIMATED
        if headline_only and base_tier.score > ConfidenceTier.ESTIMATED.score
        else base_tier
    )

    margins = {"crane_swl": dyn_margin}
    confidences = {"crane_swl": crane_tier}

    # Optional gated factors.
    if req.min_dp_class is not None:
        dp = _dp_int(info.get("dp_class"))
        margins["dp_class"] = (dp / req.min_dp_class) if dp else 0.0
        confidences["dp_class"] = ConfidenceTier.CITED if dp else ConfidenceTier.GAP
    if req.deck_area_m2 is not None:
        deck = info.get("deck_area_m2")
        margins["deck_area"] = (deck / req.deck_area_m2) if deck else 0.0
        confidences["deck_area"] = ConfidenceTier.CITED if deck else ConfidenceTier.GAP

    cs = capability_score("heavy_lift", margins, confidences)
    return SuitabilityResult(
        vessel=name,
        score=cs.score,
        defensible=cs.defensible,
        confidence=cs.confidence,
        swl_at_radius_te=round(swl, 1),
        margin=round(static_margin, 2),
        limiting_factors=cs.limiting_factors,
        source=info.get("source", ""),
    )


def rank_fleet(req: LiftRequirement, base=None) -> list[SuitabilityResult]:
    """Rank every installation vessel for the lift, best (highest score) first."""
    fleet = installation_vessels(base)
    results = [assess_vessel(name, info, req) for name, info in fleet.items()]
    results.sort(key=lambda r: r.score, reverse=True)
    return results


def assess_named(
    vessel_name: str, req: LiftRequirement, base=None
) -> Optional[SuitabilityResult]:
    """Suitability for a single named vessel (normalised match), or None."""
    target = normalize_vessel_name(vessel_name)
    for name, info in installation_vessels(base).items():
        if normalize_vessel_name(name) == target:
            return assess_vessel(name, info, req)
    return None
