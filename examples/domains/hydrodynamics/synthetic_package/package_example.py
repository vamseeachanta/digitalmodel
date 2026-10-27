# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generate and inspect a synthetic package without running a solver."""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Any, Iterator

import yaml

from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter


def _mesh_filenames(node: Any) -> Iterator[str]:
    """Read emitted mesh filenames, independently of input asset resolution."""
    if isinstance(node, dict):
        for key, value in node.items():
            if key.endswith("MeshFileName"):
                if not isinstance(value, str) or not value:
                    raise ValueError(f"Invalid emitted mesh reference: {key}")
                yield value
            else:
                yield from _mesh_filenames(value)
    elif isinstance(node, list):
        for value in node:
            yield from _mesh_filenames(value)


def build_package(
    spec_path: Path, output_dir: Path, *, output_format: str = "single"
) -> Path:
    """Use the existing converter and quality gates, then inspect its YAML.

    Args:
        spec_path: Input spec; its mesh paths resolve from its own directory.
        output_dir: Explicit package destination, preferably a fresh directory.
        output_format: Existing converter format, single or modular.

    Returns:
        Generated input file after package mesh references have been checked.

    Raises:
        FileNotFoundError: A required source or packaged mesh is absent.
        ValueError: Schema, mesh quality, or package reference checks fail.
    """
    converter = SpecConverter(spec_path)
    result = converter.convert(
        solver="orcawave", format=output_format, output_dir=output_dir
    )
    package = result.parent.resolve()
    found_reference = False
    for path in package.glob("*.yml"):
        for document in yaml.safe_load_all(path.read_text()):
            for filename in _mesh_filenames(document):
                found_reference = True
                reference = Path(filename)
                resolved = (path.parent / reference).resolve()
                if reference.is_absolute() or not resolved.is_relative_to(package):
                    raise ValueError(f"Mesh reference leaves package: {filename}")
                if not resolved.is_file():
                    raise FileNotFoundError(f"Packaged mesh missing: {filename}")
    if not found_reference:
        raise ValueError("Generated input contains no mesh references")
    for warning in converter.quality_warnings:
        print(warning)
    return result


def main() -> None:
    """Run the example with an explicit output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--spec", type=Path,
        default=Path(__file__).resolve().parent / "input/spec.yml",
        help="Input spec; defaults to the adjacent synthetic example",
    )
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--format", choices=("single", "modular"), default="single")
    args = parser.parse_args()
    try:
        result = build_package(args.spec, args.output_dir, output_format=args.format)
    except (OSError, ValueError) as error:
        parser.exit(status=1, message=f"Packaging failed: {error}\n")
    print(f"Package generated: {result.name}; solver validation has not been run.")


if __name__ == "__main__":
    main()
