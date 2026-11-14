# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Print a preliminary single-zone CP screening from the existing YAML workflow."""

from __future__ import annotations

import argparse
from pathlib import Path
import sys

import yaml

from digitalmodel.cathodic_protection.design_spec import design_from_yaml


def main(argv: list[str] | None = None) -> int:
    """Read a YAML basis and print results, returning 1 for known input errors."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--config",
        default=str(Path(__file__).resolve().with_name("synthetic_design.yml")),
        help="YAML input file (default: the adjacent synthetic example)",
    )
    args = parser.parse_args(argv)
    try:
        result = design_from_yaml(args.config)
    except (OSError, yaml.YAMLError, ValueError) as exc:
        print(f"CP design failed: {exc}", file=sys.stderr)
        return 1

    print(f"Mean current demand: {result['current_demand_A']:.1f} A")
    print(f"Lifetime charge: {result['required_charge_Ah']:.1f} Ah")
    print(f"Required net anode mass: {result['required_anode_mass_kg']:.1f} kg")
    print(result["limitation"])
    return 0


if __name__ == "__main__":
    sys.exit(main())
