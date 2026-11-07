# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Export an illustrative column curve; all assumptions are explicit in each row.

Run from the repository root with PYTHONPATH=src and --output <CSV path>.
The derivation and source are in structural.plate_capacity.perry_robertson.
Inputs are assumed illustrations, not measured material or fabrication data.
"""

import argparse
import csv
from math import pi, sqrt
from pathlib import Path

from digitalmodel.structural.plate_capacity.perry_robertson import (
    perry_robertson_strength_ratio,
)


def write_curve(output: Path) -> None:
    """Write L/r=0 to 300 in steps of 5, with the zero-length limit included."""
    youngs_modulus_mpa = 200000.0
    yield_strength_mpa = 250.0
    initial_bow_over_length = 0.001
    extreme_fibre_over_radius = 2.0
    with output.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.writer(handle, lineterminator="\n")
        writer.writerow(
            ["slenderness_ratio", "reduced_slenderness", "imperfection_parameter",
             "strength_ratio", "critical_stress_MPa", "youngs_modulus_MPa",
             "yield_strength_MPa", "initial_bow_over_length",
             "extreme_fibre_over_radius"]
        )
        for slenderness in range(0, 301, 5):
            eta = initial_bow_over_length * extreme_fibre_over_radius * slenderness
            reduced = slenderness / pi * sqrt(yield_strength_mpa / youngs_modulus_mpa)
            ratio = perry_robertson_strength_ratio(
                reduced_slenderness=reduced, imperfection_parameter=eta
            )
            writer.writerow(
                format(value, ".12g")
                for value in (
                    slenderness, reduced, eta, ratio, yield_strength_mpa * ratio,
                    youngs_modulus_mpa, yield_strength_mpa, initial_bow_over_length,
                    extreme_fibre_over_radius,
                )
            )


def main() -> None:
    """Require an explicit output location to keep temporary files out of source."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    write_curve(args.output)


if __name__ == "__main__":
    main()
