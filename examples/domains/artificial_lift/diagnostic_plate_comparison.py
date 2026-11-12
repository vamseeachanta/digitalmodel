# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline per-class comparison against the original CC0 diagnostic plate."""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Any

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    ALL_GENERATORS,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostic_plate import (
    load_diagnostic_plate,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.plate_comparison import (
    compare_diagnostic_plate,
)


def main(argv: list[str] | None = None) -> dict[str, Any]:
    """Generate samples for literal plate mappings and print measured geometry.

    Args:
        argv: Command arguments; omitted for normal command-line execution.

    Returns:
        Unrounded comparison evidence, including the supplied random seeds.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workbook", type=Path, required=True)
    parser.add_argument("--seeds", type=int, nargs="+", required=True,
                        help="Explicit unique random seeds; no calibrated sample count")
    args = parser.parse_args(argv)
    if len(set(args.seeds)) != len(args.seeds):
        raise ValueError("Random seeds must be unique")
    curves = load_diagnostic_plate(args.workbook)
    modes = sorted({curve.fault_mode for curve in curves if curve.fault_mode})
    samples = {mode: [ALL_GENERATORS[mode](seed=seed) for seed in args.seeds]
               for mode in modes}
    report = compare_diagnostic_plate(args.workbook, samples)
    report["seeds"] = args.seeds
    print("CC0 diagnostic plate shape comparison")
    print("Source DOI:", report["source_doi"])
    print("Seeds:", ", ".join(str(seed) for seed in args.seeds))
    print("label | ordinate | mode | count | median distance x 100 | nearest load label")
    for row in report["curves"]:
        if row["status"] == "MEASURED":
            # Display convention only: one decimal on the unit-coordinate
            # distance multiplied by 100, without rounding calculation inputs.
            distance = f"{100 * row['median_distance']:.1f}"
        else:
            distance = f"NOT_EVALUATED ({row['reason']})"
        print(f"{row['label']} | {row['ordinate']} | {row['fault_mode'] or '-'} | "
              f"{row['count']} | {distance} | {row['nearest_load_label'] or '-'}")
    print("Modes without literal exemplars:",
          ", ".join(report["modes_without_literal_exemplar"]))
    for key in ("acceptance", "field_accuracy"):
        print(f"{key}: {report[key]['status']} ({report[key]['reason']})")
    return report


if __name__ == "__main__":
    main()
