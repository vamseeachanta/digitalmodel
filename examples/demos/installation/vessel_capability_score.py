#!/usr/bin/env python3
# ABOUTME: Confidence-weighted vessel capability/suitability score for a lift.
# ABOUTME: Wires vessel_db crane curves + confidence tiers -> capability_score (#591/#592/#857).
"""
Vessel capability score (confidence-weighted) for a crane lift
==============================================================

Ranks the installation fleet for a lift by **capability margin weighted by data
confidence**, and flags whether each ranking is *defensible* (backed by at least
estimated-grade data on every scored field). Unlike a raw go/no-go, this surfaces
WHERE the data is weak — the gap #593 identified for client-facing GTM claims.

Crane SWL comes from the merged vessel DB (worldenergydata source of truth +
curated). Confidence is derived from the data source: worldenergydata records
(real IMO + published reach) -> cited; curated brochure records -> brochure.

Usage:
    cd digitalmodel
    uv run python examples/demos/installation/vessel_capability_score.py --lift-te 3000 --radius-m 35
"""

from __future__ import annotations

import argparse

from digitalmodel.marine_ops.vessel_db.confidence import (
    ConfidenceTier,
    capability_score,
)
from digitalmodel.marine_ops.vessel_db.loader import installation_vessels

_SOURCE_TIER = {
    "worldenergydata": ConfidenceTier.CITED,   # real IMO + published reach
    "curated": ConfidenceTier.BROCHURE,        # operator brochure / web research
}


def score_fleet(lift_te: float, radius_m: float):
    rows = []
    for name, info in installation_vessels().items():
        curve = info["crane_curve"]
        swl = curve.capacity_at_radius(radius_m)
        margin = swl / lift_te if lift_te > 0 else 0.0
        tier = _SOURCE_TIER.get(info.get("source", ""), ConfidenceTier.GENERIC)
        # headline-only SWL (no published radius curve) is weaker evidence.
        if len(curve.radii_m) <= 1 or float(curve.radii_m.max()) == 0.0:
            tier = ConfidenceTier.ESTIMATED if tier.score > ConfidenceTier.ESTIMATED.score else tier
        cs = capability_score("heavy_lift", {"crane_swl": margin}, {"crane_swl": tier})
        rows.append((name, swl, margin, cs))
    rows.sort(key=lambda r: r[3].score, reverse=True)
    return rows


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--lift-te", type=float, default=3000.0)
    p.add_argument("--radius-m", type=float, default=35.0)
    args = p.parse_args()

    rows = score_fleet(args.lift_te, args.radius_m)
    print("=" * 80)
    print(f"  VESSEL CAPABILITY SCORE (confidence-weighted) — lift {args.lift_te:.0f} te @ {args.radius_m:.0f} m")
    print("=" * 80)
    print(f"  {'score':>5} {'defensible':>10} {'confidence':>10}  {'SWL@R':>7} {'margin':>6}  vessel")
    print("-" * 80)
    for name, swl, margin, cs in rows:
        print(f"  {cs.score:>5.0f} {('yes' if cs.defensible else 'no'):>10} "
              f"{cs.confidence.value:>10}  {swl:>7.0f} {margin:>6.2f}  {name[:30]}")
    print("-" * 80)
    n_def = sum(1 for *_, cs in rows if cs.defensible)
    print(f"  {n_def}/{len(rows)} vessels meet the lift with defensible (>= estimated) data")


if __name__ == "__main__":
    main()
