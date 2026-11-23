"""
ABOUTME: Unified per-field confidence tiers + capability scoring for vessel data.

Provenance is captured three ways across the ecosystem (in-repo per-field
citations / `estimated:` / `gap`; worldenergydata per-record DATA_SOURCE +
DIMENSION_CONFIDENCE; synthetic fixtures with none). This module collapses all of
that into ONE comparable signal so capability/suitability scores can weight
themselves by how trustworthy the underlying data is.

Tiers (high -> low), each mapped to a 0..1 score:

    measured  1.00  instrument/cert/class-society/IMO-register direct
    cited     0.85  high-confidence public source with URL
    brochure  0.75  operator/builder spec sheet / brochure PDF
    estimated 0.55  documented empirical relation (e.g. kxx=0.35*beam)
    generic   0.40  class-typical / representative, no named source
    gap       0.00  not found

The tier is DERIVED from the markers already in the data — never asserted.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Optional

from digitalmodel.marine_ops.vessel_db.loader import (
    Record,
    _is_covered,
    parse_value,
)


class ConfidenceTier(str, Enum):
    MEASURED = "measured"
    CITED = "cited"
    BROCHURE = "brochure"
    ESTIMATED = "estimated"
    GENERIC = "generic"
    GAP = "gap"

    @property
    def score(self) -> float:
        return _TIER_SCORE[self]


_TIER_SCORE = {
    ConfidenceTier.MEASURED: 1.00,
    ConfidenceTier.CITED: 0.85,
    ConfidenceTier.BROCHURE: 0.75,
    ConfidenceTier.ESTIMATED: 0.55,
    ConfidenceTier.GENERIC: 0.40,
    ConfidenceTier.GAP: 0.00,
}

# Source-name hints that imply a measured/authoritative origin.
_MEASURED_HINTS = ("lloyd", "equasis", "class society", "dnv register", "abs ",
                   "imo register", "classification", "certificate")
# Citation access type -> tier.
_ACCESS_TIER = {
    "brochure-pdf": ConfidenceTier.BROCHURE,
    "estimated": ConfidenceTier.ESTIMATED,
    "public": ConfidenceTier.CITED,
    "paywalled": ConfidenceTier.CITED,
}

# Default engineering fields rolled up by record_confidence.
DEFAULT_FIELDS = ("loa", "beam", "draft", "displacement", "kxx", "kyy", "kzz")


def field_confidence(rec: Record, field_name: str) -> ConfidenceTier:
    """Confidence tier for one canonical field of a vessel record."""
    val, marker, raw_name = rec.dimension(field_name)
    if marker in ("gap", "missing"):
        return ConfidenceTier.GAP
    if marker == "estimated":
        return ConfidenceTier.ESTIMATED
    # marker == "number": trust depends on the citation backing it.
    dc = str(rec.raw_fields.get("dimension_confidence", "")).lower()
    if dc in ("generic", "default", "unknown"):
        return ConfidenceTier.GENERIC
    if dc in ("measured", "high", "certified"):
        return ConfidenceTier.MEASURED

    cited = rec.cited_fields()
    blanket = bool(cited & {"all", "geometry"})
    if not _is_covered(raw_name or field_name, cited, blanket):
        return ConfidenceTier.GENERIC  # an un-cited bare number is weak

    # Pick the covering citation and read its access type / source hints.
    for c in rec.citations:
        cfields = set(c.get("fields", []))
        if (cfields & {"all", "geometry"}) or _is_covered(raw_name or field_name, cfields, False):
            src = str(c.get("source", "")).lower()
            if any(h in src for h in _MEASURED_HINTS):
                return ConfidenceTier.MEASURED
            return _ACCESS_TIER.get(str(c.get("access", "public")).lower(),
                                    ConfidenceTier.CITED)
    return ConfidenceTier.CITED


@dataclass
class RecordConfidence:
    vessel_name: str
    score: float                         # 0..1 weighted over scored fields
    dominant_tier: ConfidenceTier
    per_field: dict[str, ConfidenceTier] = field(default_factory=dict)
    n_scored: int = 0


def record_confidence(rec: Record, fields: Optional[tuple[str, ...]] = None) -> RecordConfidence:
    """Roll up field tiers into a record-level confidence (mean tier score)."""
    fields = fields or DEFAULT_FIELDS
    per: dict[str, ConfidenceTier] = {}
    for f in fields:
        _, marker, _ = rec.dimension(f)
        if marker == "missing":
            continue  # field not present at all -> not scored (see completeness)
        per[f] = field_confidence(rec, f)
    if not per:
        return RecordConfidence(rec.name, 0.0, ConfidenceTier.GAP, {}, 0)
    score = sum(t.score for t in per.values()) / len(per)
    dominant = max(set(per.values()), key=lambda t: sum(1 for x in per.values() if x == t))
    return RecordConfidence(rec.name, round(score, 3), dominant, per, len(per))


# ---------------------------------------------------------------------------
# Capability scoring
# ---------------------------------------------------------------------------

@dataclass
class CapabilityScore:
    operation: str
    score: float                         # 0..100 (capability margin x confidence)
    confidence: ConfidenceTier           # weakest-link tier across scored fields
    limiting_factors: list[str] = field(default_factory=list)
    defensible: bool = False             # backed by >= estimated data on all key fields


def capability_score(
    operation: str,
    margins: dict[str, float],
    confidences: dict[str, ConfidenceTier],
    weights: Optional[dict[str, float]] = None,
    *,
    min_defensible: ConfidenceTier = ConfidenceTier.ESTIMATED,
) -> CapabilityScore:
    """Score a vessel for an operation from per-field margins + confidences.

    ``margins[field]`` is capability/requirement (>=1 means the vessel meets it,
    capped at 1 for scoring). The field score is ``min(margin,1) * tier_score``;
    the overall is the weighted mean x100. ``confidence`` is the weakest tier
    among the fields. ``defensible`` is True only if every field is at least
    ``min_defensible`` and all margins >= 1.
    """
    if not margins:
        return CapabilityScore(operation, 0.0, ConfidenceTier.GAP, ["no capability fields"], False)
    weights = weights or {k: 1.0 for k in margins}
    total_w = sum(weights.get(k, 1.0) for k in margins) or 1.0

    acc = 0.0
    limiting: list[str] = []
    weakest = ConfidenceTier.MEASURED
    order = list(_TIER_SCORE)  # measured..gap, descending
    for fname, margin in margins.items():
        tier = confidences.get(fname, ConfidenceTier.GAP)
        if order.index(tier) > order.index(weakest):
            weakest = tier
        capped = min(margin, 1.0)
        acc += weights.get(fname, 1.0) * capped * tier.score
        if margin < 1.0:
            limiting.append(f"{fname} margin {margin:.2f}<1")
        if tier in (ConfidenceTier.GENERIC, ConfidenceTier.GAP):
            limiting.append(f"{fname} data is {tier.value}")

    score = round(100.0 * acc / total_w, 1)
    defensible = (all(margins[k] >= 1.0 for k in margins)
                  and all(order.index(confidences.get(k, ConfidenceTier.GAP))
                          <= order.index(min_defensible) for k in margins))
    return CapabilityScore(operation, score, weakest, limiting, defensible)
