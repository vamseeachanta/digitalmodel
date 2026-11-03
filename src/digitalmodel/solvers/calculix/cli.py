# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Command-line entry point for the CalculiX pre-run model checker."""

import argparse
import json

from .model_checker import check_deck


def main(argv: list[str] | None = None) -> int:
    """Print findings and return zero only when the deck has no errors."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("deck", help="Complete CalculiX .inp analysis deck")
    parser.add_argument("--tolerance", type=float, default=1e-6,
                        help="Coincident-node distance in deck length units (default: 1e-6)")
    parser.add_argument("--json", action="store_true", help="Print structured QA evidence")
    args = parser.parse_args(argv)
    try:
        report = check_deck(args.deck, tolerance=args.tolerance)
    except ValueError as exc:
        parser.error(str(exc))
    if args.json:
        print(json.dumps(report.to_dict(), indent=2))
    else:
        print(f"Model QA: {report.status}; node tolerance: {report.tolerance:g} length units")
        for finding in report.findings:
            print(f"{finding.severity.upper()} {finding.code}: {', '.join(finding.entity_ids)}")
            print(f"  {finding.message}. Fix: {finding.fix_hint}")
    return 0 if report.status == "PASS" else 1


if __name__ == "__main__":
    raise SystemExit(main())
