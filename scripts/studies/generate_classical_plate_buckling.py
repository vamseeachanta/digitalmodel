# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Write the classical simply supported plate coefficient curve as CSV.

Run with PYTHONPATH=src and an explicit --output path. The fixed comparison
range is 0.25 <= a/b <= 10 in steps of 0.05, plus a/b=1.41 and sqrt(2).
The latter records the first mode transition. Formula and source provenance
are in digitalmodel.structural.plate_capacity.classical_elastic_buckling.
"""

import argparse
import csv
from math import sqrt
from pathlib import Path

from digitalmodel.structural.plate_capacity.classical_elastic_buckling import (
    governing_half_waves,
    plate_buckling_coefficient,
)


def write_coefficient_csv(output: Path) -> None:
    """Write deterministic dimensionless aspect ratio, mode and coefficient rows."""
    ratios = sorted({index / 20 for index in range(5, 201)} | {1.41, sqrt(2)})
    with output.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.writer(stream, lineterminator="\n")
        writer.writerow(("aspect_ratio", "half_waves", "k"))
        for ratio in ratios:
            # Full float precision retains the transition location and permits
            # independent downstream calculations from the stored abscissa.
            writer.writerow(
                (
                    repr(ratio),
                    governing_half_waves(ratio),
                    repr(plate_buckling_coefficient(ratio)),
                )
            )


def main() -> None:
    """Parse the requested CSV destination and generate the curve."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    write_coefficient_csv(args.output)


if __name__ == "__main__":
    main()
