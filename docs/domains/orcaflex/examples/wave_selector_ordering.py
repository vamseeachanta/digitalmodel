# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Print a selector-first Environment from deliberately reversed raw inputs.

All numbers are synthetic inputs, not design values. Independent calculation:
matched height = typed height = 2; matched period = typed period = 8;
extra height = raw height = 2; extra period = raw period = 8.
The method changes assignment order, with no wave-theory calculation.

Ordering source: Orcina public help, Text data files / Ordering issues,
online edition accessed 2026-10-22:
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Textdatafiles.htm
"""

import sys

import yaml

from digitalmodel.solvers.orcaflex.modular_generator.builders.context import BuilderContext
from digitalmodel.solvers.orcaflex.modular_generator.builders.environment_builder import (
    EnvironmentBuilder,
)
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec


def build_example():
    """Build matched and extra raw trains without importing a solver binding."""
    raw_train = {
        "WaveHeight": 2,
        "WavePeriod": 8,
        "WaveDirection": 0,  # Synthetic direction copied unchanged.
        "WaveType": "Airy",
        "Name": "matched",
    }
    spec = ProjectInputSpec.model_validate({
        "metadata": {
            "name": "wave ordering", "description": "synthetic ordering example",
            "structure": "generic", "operation": "in-place",
        },
        "environment": {
            # Synthetic schema inputs; no seabed calculation is performed.
            "water": {"depth": 1},
            "seabed": {"stiffness": {"normal": 0, "shear": 0}},
            "waves": {"type": "airy", "height": 2, "period": 8},
            "raw_properties": {"WaveTrains": [
                raw_train, {**raw_train, "Name": "extra"},
            ]},
        },
        "generic": {},
    })
    return EnvironmentBuilder(spec, BuilderContext()).build()


if __name__ == "__main__":
    yaml.safe_dump(build_example(), sys.stdout, sort_keys=False)
