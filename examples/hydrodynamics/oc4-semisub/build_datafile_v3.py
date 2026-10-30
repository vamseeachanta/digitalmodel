# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Resolve a mesh-bound staged specification and write both offline datafiles."""

import json
import math
from pathlib import Path

import yaml

import importlib.util


def module(name):
    """Load an adjacent offline utility."""
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(f"{name}.py"))
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded

HERE = Path(__file__).resolve().parent


def resolved_specification(upper_rad_s):
    """Keep each 0.01 grid point, exact peaks and the unrounded band endpoint."""
    recipe = json.loads((HERE / "input-specification-v3.json").read_text())
    if not math.isfinite(upper_rad_s) or upper_rad_s < 2 * math.pi / 6.59:
        raise ValueError("The supported band must include all three fatigue peaks")
    for name, upper in (("S", 0.55), ("F", upper_rad_s)):
        stage = recipe["stages"][name]
        first = round(stage["lower_rad_s"] * 100)
        last = math.floor(upper * 100)
        frequencies = {i / 100 for i in range(first, last + 1)}
        frequencies.update(2 * math.pi / tp for tp in stage["exact_periods_s"])
        frequencies.update(stage.get("exact_frequencies_rad_s", []))
        frequencies.add(upper)
        stage.update(frequencies_rad_s=sorted(frequencies), upper_rad_s=upper,
                     frequency_count=len(frequencies),
                     interior_lid=upper >= recipe["irregular_frequency_estimate_rad_s"])
    recipe["status"] = "RESOLVED_REQUIRES_DF0_AND_NATIVE_ACCEPTANCE"
    return recipe


def stage_model(stage, specification):
    """Reuse the unchanged mass basis, changing only the grid and lid request."""
    if stage not in ("S", "F"):
        raise ValueError("Unknown stage")
    inputs = dict(specification)
    inputs["frequencies_rad_s"] = specification["stages"][stage]["frequencies_rad_s"]
    deck = module("build_datafile_v2").datafile_model("l1-hull-v3.gdf", specification=inputs)
    deck["Bodies"][0]["BodyAddInteriorSurfacePanels"] = (
        "Yes" if specification["stages"][stage]["interior_lid"] else "No")
    return deck


def build_datafiles(directory, specification):
    """Write Stage S and Stage F; the packet checker will bind mesh bytes."""
    directory = Path(directory)
    for stage in ("S", "F"):
        path = directory / f"stage-{stage.lower()}-model-v3.yml"
        path.write_text(yaml.safe_dump(stage_model(stage, specification), sort_keys=False))


def probe_model(specification):
    """Keep the L2 convergence probes below the supplied irregular estimate."""
    deck = stage_model("S", specification)
    deck["PeriodOrFrequency"] = sorted([0.418879, *(2 * math.pi / tp for tp in (6.59, 7.43, 8.19))])
    deck["Bodies"][0]["BodyMeshFileName"] = "l2-hull-v3.gdf"
    return deck
