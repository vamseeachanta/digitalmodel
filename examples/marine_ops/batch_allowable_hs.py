# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Write a batch Hs table using the envelope tests' constant-gain synthetic RAOs."""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np

from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DOF,
    FrequencyData,
    HeadingData,
    RAOComponent,
    RAOSet,
)
from digitalmodel.marine_ops.batch_envelope import (
    BatchOperation,
    ResponseCriterion,
    allowable_hs_table,
)


def synthetic_raos() -> RAOSet:
    """Return the existing envelope-test grid with constant synthetic DOF gains."""
    frequencies = FrequencyData(np.array([0.001, 0.1, 0.5, 1, 5, 15]), None, 0, 0, 0)
    headings = HeadingData(np.array([0, 45, 90, 135, 180]), 0, 0, 0)
    gains = {"heave": 1.0, "roll": 0.5, "pitch": 0.3}
    components = {}
    for dof in DOF:
        name = dof.name.lower()
        magnitude = np.full((frequencies.count, headings.count), gains.get(name, 0.0))
        if name == "heave":
            magnitude[:, 2] *= 2
        components[name] = RAOComponent(
            dof, magnitude, np.zeros_like(magnitude), frequencies, headings, "",
        )
    return RAOSet(
        vessel_name="Synthetic", analysis_tool="synthetic", water_depth=1000,
        created_date="2026-06-21", **components,
    )


def example_operations() -> list[BatchOperation]:
    """Return illustrative limits only; no engineering qualification is implied."""
    point = (10.0, 0.0, 20.0)
    return [
        BatchOperation("lift", (
            ResponseCriterion("tip_heave", "heave", 1.0, "m", point_m=point),
        )),
        BatchOperation("splash_zone", (
            ResponseCriterion("tip_heave", "heave", 0.8, "m", point_m=point),
            ResponseCriterion("tip_velocity", "heave", 0.5, "m/s",
                              response="velocity", point_m=point),
        )),
        BatchOperation("set_down", (
            ResponseCriterion("tip_acceleration", "heave", 0.4, "m/s^2",
                              response="acceleration", point_m=point),
        )),
        BatchOperation("transit", (
            ResponseCriterion("heave", "heave", 3.0, "m"),
            ResponseCriterion("roll", "roll", 6.0, "deg"),
        )),
    ]


def main() -> None:
    """Write CSV to the supplied path and print the returned table."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path, help="CSV destination in an existing directory")
    args = parser.parse_args()
    table = allowable_hs_table(
        synthetic_raos(), example_operations(), tp_range_s=[8, 12],
        headings=[0, 90, 180], spectrum_type="jonswap", gamma=3.3, csv_path=args.output,
    )
    print(table.to_string(index=False, float_format=lambda value: f"{value:.1f}"))


if __name__ == "__main__":
    main()
