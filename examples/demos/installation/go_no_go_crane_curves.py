#!/usr/bin/env python3
# ABOUTME: Go/No-Go crane-lift screen driven by REAL vessel-database crane curves.
# ABOUTME: Wires marine_ops.vessel_db.installation_vessels() into a DNV crane UC check.
"""
Go/No-Go crane screen across the real installation fleet
========================================================

Wires the curated vessel database (``data/vessels/``) into a crane lift
Go/No-Go decision. For a given lift weight and crane working radius, it reads
each real installation vessel's published crane capacity curve
(``CraneCurve.capacity_at_radius``) and evaluates DNV-RP-H103 crane utilisation:

  - Static UC   = lift / SWL(radius)              (limit 0.70)
  - Dynamic UC  = lift * DAF / SWL(radius)        (limit 1.00)

then ranks the fleet GO / MARGINAL / NO_GO.

Crane SWLs come from cited public sources (see data/vessels/raw/install__crane_deck.json
and COVERAGE.md). Where a vessel publishes only a headline SWL (no radius points),
``capacity_at_radius`` returns that headline value — flagged in the output.

Usage:
    cd digitalmodel
    uv run python examples/demos/installation/go_no_go_crane_curves.py
    uv run python examples/demos/installation/go_no_go_crane_curves.py --lift-te 2500 --radius-m 40
"""

from __future__ import annotations

import argparse
import json
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Optional

from digitalmodel.marine_ops.installation.go_no_go import (
    CriterionState,
    DecisionState,
    _check_criterion,
)
from digitalmodel.marine_ops.vessel_db import installation_vessels

OUTPUT_DIR = Path(__file__).resolve().parent / "output"


@dataclass
class VesselVerdict:
    vessel: str
    vessel_type: str
    swl_at_radius_te: float
    static_uc: float
    dynamic_uc: float
    decision: str
    headline_only: bool  # True when no published radius curve (single SWL point)
    note: str = ""


def screen_fleet(
    lift_te: float,
    radius_m: float,
    *,
    daf: float = 1.30,
    static_uc_limit: float = 0.70,
    base: Optional[Path] = None,
) -> list[VesselVerdict]:
    """Run the crane Go/No-Go screen across every DB installation vessel.

    Returns one ``VesselVerdict`` per vessel, sorted best (lowest dynamic UC)
    first. A vessel whose published curve cannot carry the lift at the radius
    (dynamic UC > 1) is NO_GO; static UC over its limit but dynamic within is
    MARGINAL.
    """
    fleet = installation_vessels(base)
    verdicts: list[VesselVerdict] = []
    for name, info in fleet.items():
        curve = info["crane_curve"]
        swl = curve.capacity_at_radius(radius_m)
        headline_only = len(curve.radii_m) <= 1 or float(curve.radii_m.max()) == 0.0
        static_uc = lift_te / swl if swl > 0 else float("inf")
        dynamic_uc = lift_te * daf / swl if swl > 0 else float("inf")

        static = _check_criterion(
            name="Crane SWL utilisation (static)", value=static_uc,
            limit=static_uc_limit, unit="ratio", above_is_safe=False,
            reference="DNV-RP-H103 Section 5.4.1",
        )
        dynamic = _check_criterion(
            name="Crane dynamic capacity utilisation", value=dynamic_uc,
            limit=1.0, unit="ratio", above_is_safe=False,
            reference="DNV-RP-H103 Section 5.4.2",
        )
        states = {static.state, dynamic.state}
        if CriterionState.FAIL in states:
            decision = DecisionState.NO_GO
        elif CriterionState.WARNING in states:
            decision = DecisionState.MARGINAL
        else:
            decision = DecisionState.GO

        note = ""
        if headline_only:
            note = "headline SWL only (no published radius curve) — radius not applied"
        elif radius_m > float(curve.radii_m.max()):
            note = (f"radius {radius_m:.0f} m beyond published curve "
                    f"(max {curve.radii_m.max():.0f} m); extrapolated flat")

        verdicts.append(VesselVerdict(
            vessel=name,
            vessel_type=info["vessel_type"],
            swl_at_radius_te=round(swl, 1),
            static_uc=round(static_uc, 3),
            dynamic_uc=round(dynamic_uc, 3),
            decision=decision.value,
            headline_only=headline_only,
            note=note,
        ))

    order = {"GO": 0, "MARGINAL": 1, "NO_GO": 2}
    verdicts.sort(key=lambda v: (order[v.decision], v.dynamic_uc))
    return verdicts


def _print_table(lift_te, radius_m, daf, verdicts) -> None:
    icon = {"GO": "[GO]      ", "MARGINAL": "[MARGINAL]", "NO_GO": "[NO_GO]   "}
    print("=" * 84)
    print("  CRANE GO/NO-GO SCREEN — real vessel-database crane curves")
    print("=" * 84)
    print(f"  Lift: {lift_te:.0f} te   Working radius: {radius_m:.0f} m   DAF: {daf:.2f}")
    print("-" * 84)
    print(f"  {'decision':10} {'vessel':28} {'SWL@R te':>9} {'stat UC':>8} {'dyn UC':>7}")
    print("-" * 84)
    for v in verdicts:
        print(f"  {icon[v.decision]} {v.vessel[:28]:28} {v.swl_at_radius_te:>9.0f} "
              f"{v.static_uc:>8.3f} {v.dynamic_uc:>7.3f}"
              + ("  *" if v.note else ""))
    notes = {v.vessel: v.note for v in verdicts if v.note}
    if notes:
        print("-" * 84)
        for name, note in notes.items():
            print(f"  * {name}: {note}")
    n_go = sum(1 for v in verdicts if v.decision == "GO")
    print("-" * 84)
    print(f"  {n_go}/{len(verdicts)} vessels GO for this lift")
    print("=" * 84)


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--lift-te", type=float, default=1200.0, help="Lift weight [te]")
    p.add_argument("--radius-m", type=float, default=30.0, help="Crane working radius [m]")
    p.add_argument("--daf", type=float, default=1.30, help="Dynamic amplification factor")
    p.add_argument("--json", action="store_true", help="Also write a JSON results file")
    args = p.parse_args()

    verdicts = screen_fleet(args.lift_te, args.radius_m, daf=args.daf)
    _print_table(args.lift_te, args.radius_m, args.daf, verdicts)

    if args.json:
        OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
        out = OUTPUT_DIR / "go_no_go_crane_curves.json"
        out.write_text(json.dumps({
            "lift_te": args.lift_te, "radius_m": args.radius_m, "daf": args.daf,
            "verdicts": [asdict(v) for v in verdicts],
        }, indent=2))
        print(f"\nWrote {out}")


if __name__ == "__main__":
    main()
