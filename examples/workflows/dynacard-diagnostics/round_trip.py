# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run the synthetic example through the default surface-to-pump solver.

Inputs are the assumed demonstration values in input.yml. Output is a
numerical consistency comparison, not a field accuracy or equipment rating.
"""

import json
from pathlib import Path

import numpy as np
import yaml

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    get_generator,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow


def run_example() -> dict:
    """Return dimensions and round-trip error without writing report files."""
    cfg = yaml.safe_load(Path(__file__).with_name("input.yml").read_text())
    cfg["report"]["html"] = False
    settings = cfg["synthetic_card"]
    original = get_generator(settings["mode"])(seed=settings["seed"])
    result = DynacardWorkflow().router(cfg)
    surface = result["well_data"]["surface_card"]
    recovered = result["results"]["downhole_card"]
    load = np.asarray(original.load)
    # NRMSE is RMS(load error) divided by the original peak-to-peak load.
    # This defines a comparison quantity; it supplies no acceptance limit.
    load_nrmse = np.sqrt(np.mean((np.asarray(recovered["load"]) - load) ** 2)) / np.ptp(load)
    return {
        "solver": result["results"]["solver_method"],
        "classification": result["artificial_lift"]["classification"],
        "pump_stroke_in": round(float(np.ptp(original.position)), 1),
        "surface_stroke_in": round(float(np.ptp(surface["position"])), 1),
        "recovered_stroke_in": round(float(np.ptp(recovered["position"])), 1),
        "load_nrmse_pct": round(float(load_nrmse) * 100, 1),
    }


if __name__ == "__main__":
    print(json.dumps(run_example(), indent=2))
