# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compute the synthetic two-layer annulus case defined in README.md."""

from __future__ import annotations

import argparse
import json
from pathlib import Path


def build_example() -> dict:
    """Return full-wall areas and dry masses using the cross-section API."""
    from digitalmodel.subsea.flexible_pipes.cross_section import (
        FlexiblePipeSection,
        Layer,
        LayerType,
    )

    # Source: README.md, Synthetic section; dimensions in m, densities in kg/m^3.
    section = FlexiblePipeSection(
        [
            Layer(LayerType.PRESSURE_SHEATH, 0.200, 0.005, 950.0),
            Layer(LayerType.OUTER_SHEATH, 0.210, 0.010, 1200.0),
        ]
    )
    return {
        "layers": [
            {
                "layer_type": layer.layer_type.value,
                "annular_area_m2": layer.annular_area,
                "dry_mass_per_length_kg_per_m": layer.mass_per_length(),
            }
            for layer in section.layers
        ],
        "dry_mass_per_length_kg_per_m": section.dry_mass_per_length(),
        "NOT_EVALUATED": [
            "stiffness interaction",
            "API compliance",
            "design qualification",
        ],
    }


def main(argv: list[str] | None = None) -> None:
    """Print strict JSON, or write it to an explicitly requested output file."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, help="write JSON here instead of stdout")
    args = parser.parse_args(argv)
    payload = json.dumps(build_example(), allow_nan=False, sort_keys=True) + "\n"
    if args.output is None:
        print(payload, end="")
    else:
        args.output.write_text(payload, encoding="utf-8")


if __name__ == "__main__":
    main()
