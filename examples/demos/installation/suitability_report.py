#!/usr/bin/env python3
# ABOUTME: Generate a confidence-badged vessel suitability report (HTML + markdown).
# ABOUTME: Thin CLI over marine_ops.installation.suitability_report (#883/#591/#592).
"""
Vessel suitability report
=========================

Renders a ranked, confidence-badged fleet-suitability report for a lift scenario
(HTML + markdown) over the merged vessel DB fleet — the GTM-consumable artifact
on top of the suitability API.

Usage:
    cd digitalmodel
    uv run python examples/demos/installation/suitability_report.py --lift-te 4000 --radius-m 40
    uv run python examples/demos/installation/suitability_report.py --lift-te 3000 --radius-m 35 --min-dp 2
"""

from __future__ import annotations

import argparse
from pathlib import Path

from digitalmodel.marine_ops.installation.suitability_report import (
    build_report,
    to_html,
    to_markdown,
)
from digitalmodel.marine_ops.installation.vessel_suitability import LiftRequirement

OUTPUT_DIR = Path(__file__).resolve().parent / "output"


def main() -> None:
    p = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    p.add_argument("--lift-te", type=float, default=4000.0)
    p.add_argument("--radius-m", type=float, default=40.0)
    p.add_argument("--min-dp", type=int, default=None)
    p.add_argument("--deck-area", type=float, default=None)
    p.add_argument("--top", type=int, default=20, help="rows to render (0 = all)")
    args = p.parse_args()

    req = LiftRequirement(
        weight_te=args.lift_te,
        radius_m=args.radius_m,
        min_dp_class=args.min_dp,
        deck_area_m2=args.deck_area,
    )
    title = f"Vessel suitability — {args.lift_te:.0f} te @ {args.radius_m:.0f} m"
    report = build_report(req, title=title)
    limit = args.top or None

    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    md = OUTPUT_DIR / "suitability_report.md"
    html = OUTPUT_DIR / "suitability_report.html"
    md.write_text(to_markdown(report, limit=limit))
    html.write_text(to_html(report, limit=limit))

    print(to_markdown(report, limit=min(limit or 10, 10)))
    print(f"\nWrote {md}\n      {html}")


if __name__ == "__main__":
    main()
