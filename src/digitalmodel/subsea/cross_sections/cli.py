"""CLI for deterministic subsea cross-section reports."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from digitalmodel.subsea.cross_sections.reporting import write_report_files


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", default="docs/subsea/cross_sections", type=Path)
    parser.add_argument("--format", choices=("md", "html", "all"), default="all")
    args = parser.parse_args(argv)

    formats = {"md", "html"} if args.format == "all" else {args.format}
    try:
        generated = write_report_files(args.output_dir, formats)
    except Exception as exc:  # pragma: no cover - exercised by CLI users
        print(f"report generation failed: {exc}", file=sys.stderr)
        return 1
    for path in generated:
        print(path.as_posix())
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
