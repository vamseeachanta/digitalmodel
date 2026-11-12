# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Print a synthetic pressure-transfer comparison; no solver is called."""

from dataclasses import asdict
import json
from pathlib import Path

from digitalmodel.structural.pressure_transfer import (
    SurfacePressure,
    TransferCriteria,
    compare_transfer,
    pressure_resultant,
)


def run_example():
    """Use the declared common coordinates and limits in the adjacent input file."""
    data = json.loads(Path(__file__).with_name("input.json").read_text())

    def integrate(name):
        return pressure_resultant(
            [SurfacePressure(**patch) for patch in data[name]],
            origin_m=data["origin_m"],
            frame=data["frame"],
            case_id=data["case_id"],
        )

    source = integrate("source")
    mapped = integrate("mapped")
    criteria = TransferCriteria(**data["criteria"])
    return {
        "basis": data["basis"],
        "source": asdict(source),
        "conserved": asdict(compare_transfer(source, mapped, criteria)),
        "shifted": asdict(compare_transfer(source, integrate("shifted"), criteria)),
        "missing_criteria": asdict(compare_transfer(source, mapped, None)),
        "engineering_readiness": "NOT_EVALUATED",
    }


if __name__ == "__main__":
    print(json.dumps(run_example(), indent=2, allow_nan=False))
