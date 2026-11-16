# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Persist the experimental dimensional screen to explicit JSON and HTML files."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

import yaml

from digitalmodel.asset_integrity.assessment.screening_report import store_report
from digitalmodel.asset_integrity.mooring_chain_degradation import router


def main(argv=None) -> int:
    """Run one inspection; refuse input collisions and existing output files."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path, help="UTF-8 inspection YAML")
    parser.add_argument(
        "--output", required=True, type=Path, help="New JSON output file"
    )
    args = parser.parse_args(argv)
    try:
        output = args.output.absolute()
        if output.suffix != ".json":
            raise ValueError("--output must name a new .json file")
        report = output.with_suffix(".html")
        for destination in (output, report):
            if destination.resolve() == args.input.resolve():
                raise ValueError("output destination collides with input")
            if destination.exists() or destination.is_symlink():
                raise ValueError("output destination already exists; choose a new name")
        cfg = yaml.safe_load(args.input.read_text(encoding="utf-8"))
        if not isinstance(cfg, dict):
            raise ValueError("inspection YAML must be a mapping")
        payload = router(cfg)["mooring_chain_degradation"]
        # Serialize before any writes. Unknown quantities remain null; no report
        # storage path is included in the portable inspection/result payload.
        serialized = (
            json.dumps(payload, ensure_ascii=False, allow_nan=False, indent=2) + "\n"
        )
        output.parent.mkdir(parents=True, exist_ok=True)
        # Exclusive creation also refuses outputs created after the preflight.
        # Paired writes are not transactional: an I/O failure can leave partial
        # files, which remain visible and will be refused by a subsequent run.
        with output.open("x", encoding="utf-8") as stream:
            with report.open("x", encoding="utf-8"):
                pass
            store_report(payload, {"output_dir": str(output.parent)}, report.name)
            stream.write(serialized)
    except (OSError, UnicodeError, ValueError, TypeError, yaml.YAMLError) as exc:
        parser.error(str(exc))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
