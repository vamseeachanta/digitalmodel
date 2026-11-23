#!/usr/bin/env python3
# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the public reference mooring analysis from retained numerical results."""
from __future__ import annotations

import argparse
from hashlib import sha256
import json
from pathlib import Path

from digitalmodel.marine_ops.fowt_quasi_static.report import BASIS, INPUT, SOURCES, build_report


def retained_results(root: Path, manifest: dict) -> dict:
    """Return retained results only after every required input digest matches."""
    digests = manifest.get("input_digests")
    if not isinstance(digests, dict) or set(digests) != {BASIS, INPUT, SOURCES}:
        raise ValueError("A complete input digest set is required")
    root = Path(root).resolve()
    for name, digest in digests.items():
        source = (root/name).resolve()
        if root not in source.parents:
            raise ValueError("Manifest input path escapes the source tree")
        try:
            actual = sha256(source.read_bytes()).hexdigest()
        except OSError as error:
            raise ValueError("Manifest input cannot be read") from error
        if actual != digest:
            raise ValueError("Manifest input digest mismatch")
    results: dict = manifest["results"]
    return results


def main() -> None:
    """Compute or reuse retained results, then emit the report and manifest."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--scratch", type=Path)
    parser.add_argument("--compute", action="store_true")
    parser.add_argument("--from-manifest", type=Path)
    parser.add_argument("--output-dir", type=Path)
    arguments = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    if arguments.from_manifest:
        if arguments.compute:
            parser.error("--compute and --from-manifest are mutually exclusive")
        manifest = json.loads(arguments.from_manifest.read_text())
        results = retained_results(root, manifest)
    else:
        if arguments.scratch is None:
            parser.error("--scratch is required without --from-manifest")
        scratch = arguments.scratch.resolve()
        if scratch == root or root in scratch.parents:
            parser.error("Numerical scratch must be outside the source tree")
        scratch.mkdir(parents=True, exist_ok=True)
        from digitalmodel.marine_ops.fowt_quasi_static.workflow import load_basis, panel_runs, summarise
        if arguments.compute:
            basis = load_basis(root)
            panel_runs(basis, scratch)
        results = summarise(root, scratch)
    build_report(root, results, arguments.output_dir or root/"docs/reports")


if __name__ == "__main__":
    main()
