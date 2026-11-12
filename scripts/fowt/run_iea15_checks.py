"""Run the licence-free checks for the IEA 15 MW / VolturnUS-S fowt model.

    python scripts/fowt/run_iea15_checks.py

Writes ``docs/domains/orcaflex/examples/fowt/iea15-volturnus-s/results.json``:
the computed values, each comparison with its comparator, source and
tolerance, and the atlas readout. No licensed solver is used.
"""
from __future__ import annotations

import json
import math
from pathlib import Path

import numpy as np
import pandas as pd

from digitalmodel.floating_wind.fowt_static_checks import run_checks
from digitalmodel.floating_wind.iea15_orcaflex_spec import build_spec, load_sources

ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / "docs" / "domains" / "orcaflex" / "examples" / "fowt" / "iea15-volturnus-s" / "results.json"
PREVIOUS = ROOT / "docs" / "reports" / "fowt-volturnus-mooring-analysis.manifest.json"
ATLAS = ROOT / "atlases" / "fowt_mooring"
ALLEN = "Allen et al. (2020), NREL/TP-5000-76773"
BEM = "repository quasi-static analysis (#2473), docs/reports/fowt-volturnus-mooring-analysis.manifest.json"


def compare(quantity: str, unit: str, computed: float, comparator: float, source: str,
            tolerance: float | None) -> dict:
    computed, comparator = float(computed), float(comparator)
    deviation = (computed - comparator) / comparator
    return {"quantity": quantity, "unit": unit, "computed": computed, "comparator": comparator,
            "comparator_source": source, "tolerance": tolerance, "deviation": deviation,
            "within_tolerance": None if tolerance is None else bool(abs(deviation) <= tolerance)}


def atlas_readout(offset: float) -> dict:
    default = (ATLAS / "default.txt").read_text().strip()
    grid = pd.read_parquet(ATLAS / default / "grid.parquet")
    out = {}
    for limit, rows in grid.groupby("mbr_limit_m"):
        rows = rows.sort_values("watch_circle_radius")
        out[f"{limit:g}"] = float(np.interp(offset, rows["watch_circle_radius"],
                                            rows["mbr_utilisation"]))
    return {"atlas": f"atlases/fowt_mooring/{default}", "watch_circle_radius": offset,
            "mbr_utilisation_by_mbr_limit_m": out}


def main() -> int:
    basis, data = load_sources()
    spec = build_spec(basis, data)
    turbine = data["turbine"]
    thrust = basis["turbine"]["rated_rotor_thrust"]["value"]
    hub = [turbine["hub_overhang"]["value"], 0.0, turbine["hub_height"]["value"]]
    tilt = turbine["shaft_tilt"]["value"]
    r = run_checks(spec, data, thrust, hub, tilt)
    previous = json.loads(PREVIOUS.read_text(encoding="utf-8"))["results"]
    published = basis["published"]
    periods = r["natural"]["periods"]
    still, loaded, held = r["still_water"], r["loaded_free"], r["loaded_surge_only"]
    operating = next(c for c in previous["cases"] if c["sea"] == "operating"
                     and c["state"] == "intact" and c["heading"] == 0.0)
    hydro = spec["fowt"]["hull"]["hydrostatic_stiffness_excluding_gravity"]
    comparisons = [
        compare("Displaced volume", "m3", spec["fowt"]["hull"]["displaced_volume"],
                published["displacement"]["value"], f"{ALLEN}, Table 2", 0.01),
        compare("Heave hydrostatic stiffness", "N/m", hydro[0][0] * 1000.0,
                published["hydrostatic_stiffness"]["value"][2], f"{ALLEN}, Table 3", 0.02),
        compare("Pitch hydrostatic stiffness, weight term excluded", "N m/rad",
                hydro[2][2] * 1000.0, published["hydrostatic_stiffness"]["value"][4],
                f"{ALLEN}, Table 3", 0.03),
    ]
    for k, line in enumerate(still["lines"]):
        comparisons.append(compare(f"Still-water fairlead tension, line {k + 1}", "N",
                                   line["tension"], basis["mooring"]["published_pretension"]["value"],
                                   f"{ALLEN}, Table 6", 0.02))
    index = {"surge": 0, "heave": 2, "pitch": 4, "yaw": 5}
    tolerance = {"surge": 0.10, "heave": 0.05, "pitch": 0.05, "yaw": 0.10}
    for mode, i in index.items():
        comparisons.append(compare(f"Natural period, {mode}", "s", periods[mode],
                                   published["natural_periods"]["value"][i],
                                   f"{ALLEN}, Table 11 (frequency to 0.001 Hz, inverted)",
                                   tolerance[mode]))
        comparisons.append(compare(f"Natural period, {mode}", "s", periods[mode],
                                   previous["periods"]["at_tower_top"][i], BEM, 0.02))
    comparisons.append(compare("Mean surge at rated thrust, rotations and draft held", "m",
                               held["position"][0], operating["mean"][0], BEM, 0.05))
    result = {
        "description": "Licence-free checks of the generated IEA 15 MW / VolturnUS-S fowt "
                       "model (rigid representation). SI units.",
        "gravity": spec["fowt"]["gravity"],
        "rated_thrust": thrust,
        "hub_position": hub,
        "shaft_tilt_deg": tilt,
        "net_buoyancy_minus_weight": r["net_buoyancy"],
        "still_water": {
            "surge": still["position"][0], "heave": still["position"][2],
            "pitch_deg": math.degrees(still["position"][4]),
            "lines": still["lines"],
        },
        "natural_periods": periods,
        "mooring_stiffness_diagonal": np.diag(r["natural"]["mooring_stiffness"]).tolist(),
        "rated_thrust_free": {
            "surge": loaded["position"][0], "heave": loaded["position"][2],
            "pitch_deg": math.degrees(loaded["position"][4]), "lines": loaded["lines"],
        },
        "rated_thrust_surge_only": {"surge": held["position"][0], "lines": held["lines"]},
        "comparisons": comparisons,
        "atlas": atlas_readout(loaded["position"][0]),
    }
    OUT.write_text(json.dumps(result, indent=1) + "\n", encoding="utf-8", newline="\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
