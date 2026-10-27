# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Display a best-effort specification and its reverse-extraction report."""

from __future__ import annotations

import argparse
import json
from dataclasses import asdict
from itertools import combinations
from pathlib import Path, PurePath

from digitalmodel.solvers.orcaflex.format_converter.protocols import ConversionReport
from digitalmodel.solvers.orcaflex.format_converter.single_to_spec import (
    SingleToSpecConverter,
)


def format_report(report: ConversionReport) -> str:
    """Serialize the report with portable forward-slash path metadata."""
    return json.dumps(asdict(report), default=PurePath.as_posix, indent=2)


def main() -> int:
    """Convert explicit input/output paths and display the report with the spec.

    Returns:
        A successful exit status when extraction succeeds, otherwise failure.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True, help="Strict YAML input")
    parser.add_argument("--spec-output", type=Path, required=True, help="Extracted spec")
    parser.add_argument("--report-output", type=Path, required=True, help="JSON report")
    args = parser.parse_args()
    paths = (args.input, args.spec_output, args.report_output)
    if len({path.resolve() for path in paths}) != len(paths) or any(
        first.exists() and second.exists() and first.samefile(second)
        for first, second in combinations(paths, 2)
    ):
        parser.error("Input, spec output and report output paths must be distinct")

    report = SingleToSpecConverter().convert(
        source=args.input, target=args.spec_output,
    )
    report_text = format_report(report)
    args.report_output.parent.mkdir(parents=True, exist_ok=True)
    args.report_output.write_text(report_text + "\n", encoding="utf-8", newline="\n")
    print("Conversion report (JSON):")
    print(report_text)
    if report.success:
        print("\nProduced spec (review before use):")
        print(args.spec_output.read_text(encoding="utf-8"), end="")
    return 0 if report.success else 1


if __name__ == "__main__":
    raise SystemExit(main())
