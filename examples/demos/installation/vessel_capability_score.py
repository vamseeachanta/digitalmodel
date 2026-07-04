#!/usr/bin/env python3
# ABOUTME: Confidence-weighted vessel capability/suitability score for a lift.
# ABOUTME: Thin CLI over marine_ops.installation.vessel_suitability (#881/#591/#592).
"""
Vessel capability score (confidence-weighted) for a crane lift
==============================================================

Ranks the installation fleet for a lift by **capability margin weighted by data
confidence**, flagging whether each ranking is *defensible* (>= estimated-grade
data on every scored field) — the gap #593 identified for client-facing claims.

Thin wrapper over the reusable API
``marine_ops.installation.vessel_suitability`` (rank_fleet / LiftRequirement),
which scores over the merged vessel DB fleet (worldenergydata source of truth +
curated).

Usage:
    cd digitalmodel
    uv run python examples/demos/installation/vessel_capability_score.py --lift-te 3000 --radius-m 35
    uv run python examples/demos/installation/vessel_capability_score.py --lift-te 4000 --radius-m 40 --min-dp 2
"""

from __future__ import annotations

import argparse

from digitalmodel.marine_ops.installation.vessel_suitability import (
    LiftRequirement,
    rank_fleet,
)


def score_fleet(lift_te: float, radius_m: float, min_dp=None, deck_area=None):
    """Ranked SuitabilityResult list for a lift (kept for demo/test convenience)."""
    return rank_fleet(
        LiftRequirement(
            weight_te=lift_te,
            radius_m=radius_m,
            min_dp_class=min_dp,
            deck_area_m2=deck_area,
        )
    )


def main() -> None:
    p = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    p.add_argument("--lift-te", type=float, default=3000.0)
    p.add_argument("--radius-m", type=float, default=35.0)
    p.add_argument("--min-dp", type=int, default=None, help="required DP class (2/3)")
    p.add_argument(
        "--deck-area", type=float, default=None, help="required deck area m2"
    )
    args = p.parse_args()

    rows = score_fleet(args.lift_te, args.radius_m, args.min_dp, args.deck_area)
    print("=" * 80)
    print(
        f"  VESSEL CAPABILITY SCORE (confidence-weighted) — lift {args.lift_te:.0f} te @ {args.radius_m:.0f} m"
    )
    print("=" * 80)
    print(
        f"  {'score':>5} {'defensible':>10} {'confidence':>10}  {'SWL@R':>7} {'margin':>6}  vessel"
    )
    print("-" * 80)
    for r in rows:
        print(
            f"  {r.score:>5.0f} {('yes' if r.defensible else 'no'):>10} "
            f"{r.confidence.value:>10}  {r.swl_at_radius_te:>7.0f} {r.margin:>6.2f}  {r.vessel[:30]}"
        )
    print("-" * 80)
    n_def = sum(1 for r in rows if r.defensible)
    print(
        f"  {n_def}/{len(rows)} vessels meet the lift with defensible (>= estimated) data"
    )


if __name__ == "__main__":
    main()
