# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Compute the two public-case sheath annuli cited in README.md."""

from __future__ import annotations

import argparse
import json
from pathlib import Path


def build_example() -> dict:
    """Return sheath areas and a dry-mass subtotal using the layer API."""
    from digitalmodel.subsea.flexible_pipes.cross_section import (
        Layer,
        LayerType,
    )

    # DOI 10.1007/s12204-019-2064-8 (2019), section 1, Table 1, p. 358:
    # Fluid barrier (ID=70.2 mm, OD=80.0 mm, rho=1080 kg/m^3);
    # Outer sheath (ID=110.5 mm, OD=111.5 mm, rho=800 kg/m^3).
    # Convert mm to m; t=(OD-ID)/2. The selected layers are not contiguous.
    layers = [
        Layer(LayerType.PRESSURE_SHEATH, 0.0702, (0.0800 - 0.0702) / 2, 1080.0),
        Layer(LayerType.OUTER_SHEATH, 0.1105, (0.1115 - 0.1105) / 2, 800.0),
    ]
    return {
        "scope": "fluid barrier and outer sheath only",
        "layers": [
            {
                "layer_type": layer.layer_type.value,
                "annular_area_m2": layer.annular_area,
                "dry_mass_per_length_kg_per_m": layer.mass_per_length(),
            }
            for layer in layers
        ],
        "dry_mass_per_length_kg_per_m": sum(layer.mass_per_length() for layer in layers),
        "NOT_EVALUATED": [
            "stiffness interaction",
            "API compliance",
            "design qualification",
            "complete-pipe dry mass",
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
        args.output.write_text(payload, encoding="utf-8", newline="\n")


if __name__ == "__main__":
    main()
