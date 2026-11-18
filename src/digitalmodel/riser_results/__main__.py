# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Command line entry point for completed simulation extraction."""

from __future__ import annotations

import argparse
from pathlib import Path

from .exporter import export_results
from .native import load_adapter
from .schema import Document, read_document


class _RuntimeAdapter:
    def read(self, result_file: Path, case_row: Document, options: Document) -> Document:
        return load_adapter().read(result_file, case_row, options)


def main(argv: list[str] | None = None) -> int:
    """Export using an explicit manifest or the simulation's export sidecar."""
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest="command", required=True)
    export = sub.add_parser("export")
    export.add_argument("--sim", type=Path, required=True)
    export.add_argument("--case", required=True)
    export.add_argument("--out", type=Path, required=True)
    export.add_argument("--manifest", type=Path)
    args = parser.parse_args(argv)
    try:
        manifest_path = args.manifest or args.sim.with_suffix(".export.json")
        manifest = read_document(manifest_path)
        row = manifest["case_row"]
        if row["case_id"] != args.case:
            raise ValueError("requested case differs from frozen manifest row")
        input_file = Path(manifest["input_file"])
        if not input_file.is_absolute():
            input_file = manifest_path.parent / input_file
        paths = export_results(args.sim, input_file, row, manifest["extraction"], args.out, _RuntimeAdapter())
        print(f"Exported {len(paths)} validated exchange documents.")
        return 0
    except (ValueError, KeyError, TypeError, OSError, ImportError) as error:
        # Never echo source paths or raw native errors into public logs.
        print(f"Export failed ({type(error).__name__}); inspect the input manifest and native result configuration.")
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
