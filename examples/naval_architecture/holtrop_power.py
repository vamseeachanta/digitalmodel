#!/usr/bin/env python3
# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run a synthetic resistance and effective-power screening example.

Usage and input provenance: docs/examples/holtrop-power.md.
Uses the package's existing method equations and physical assumptions only.
"""

import argparse
import json
import math
from pathlib import Path
from typing import Any

from digitalmodel.naval_architecture import holtrop_coefficients as hc
from digitalmodel.naval_architecture import holtrop_mennen as hm

# Source: the explicitly synthetic input table in the usage page. These values
# are chosen for demonstration, not derived from measured or named hull data.
SYNTHETIC_INPUTS: dict[str, Any] = {
    "lwl": 100.0,
    "beam": 16.0,
    "draft": 5.0,
    "cb": 0.60,
    "cm": 0.96,
    "cwp": 0.72,
    "cp": 0.625,  # cb/cm, by hand: 0.60/0.96.
    "lcb_pct": -1.0,
    "cstern": 0,
    "speed_ms": 5.0,
    "abt": 16.0,
    "tf": 5.0,
    "hb": 1.0,
    "at": 2.0,
    "appendage_areas": [8.0],
    "appendage_factors": [1.5],
}


def build_result() -> dict[str, Any]:
    """Return component forces in N and effective power in W without writing."""
    inputs = SYNTHETIC_INPUTS
    area = hm.wetted_surface_holtrop(**{
        key: inputs[key] for key in ("lwl", "beam", "draft", "cb", "cm", "cwp", "abt")
    })
    factor = hm.form_factor_k1(**{
        key: inputs[key] for key in ("lwl", "beam", "draft", "cp", "lcb_pct", "cstern")
    })
    friction = hm.frictional_resistance(inputs["lwl"], inputs["speed_ms"], area)
    wave = hm.wave_resistance(**{
        key: value for key, value in inputs.items()
        if key not in ("appendage_areas", "appendage_factors")
    })
    ca = hm.correlation_allowance(inputs["lwl"], inputs["cb"], inputs["draft"])
    # Dynamic-pressure definition: q = rho*V^2/2; RA = q*S*CA, as in
    # holtrop_mennen.total_resistance. No new empirical coefficient is used.
    components = {
        "viscous_n": friction * factor,
        "appendage_n": hm.appendage_resistance(
            inputs["speed_ms"], inputs["appendage_areas"],
            inputs["appendage_factors"], inputs["lwl"],
        ),
        "wave_n": wave,
        "bulb_n": hm.bulbous_bow_resistance(
            inputs["speed_ms"], inputs["abt"], inputs["tf"], inputs["hb"],
        ),
        "transom_n": hm.transom_resistance(
            inputs["speed_ms"], inputs["at"], inputs["beam"],
            inputs["cwp"], inputs["lwl"],
        ),
        "correlation_n": hc.RHO_SW * inputs["speed_ms"] ** 2 * area * ca / 2,
    }
    total = hm.total_resistance(**inputs)
    power = hm.effective_power(total, inputs["speed_ms"])
    # Independent bookkeeping: stable summation and direct multiplication.
    # The tolerance is a chosen floating-point guard, not an accuracy criterion.
    if not math.isclose(total, math.fsum(components.values()), rel_tol=1e-12):
        raise ArithmeticError("Component force balance failed")
    if not math.isclose(power, total * inputs["speed_ms"], rel_tol=1e-12):
        raise ArithmeticError("Effective power balance failed")
    return {
        "synthetic": True,
        "limitation": "Screening only; no real-hull numerical validation.",
        "inputs": inputs,
        "physical_assumptions": {
            "density_kg_m3": hc.RHO_SW,
            "kinematic_viscosity_m2_s": hc.NU_SW,
            "gravity_m_s2": hc.G,
        },
        "wetted_surface_m2": area,
        "form_factor_1_plus_k1": factor,
        "friction_n": friction,
        "froude_number": inputs["speed_ms"] / math.sqrt(hc.G * inputs["lwl"]),
        "components_n": components,
        "total_resistance_n": total,
        "effective_power_w": power,
    }


def main() -> None:
    """Write one result file only inside the caller-selected directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    result = build_result()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    # Exclusive creation preserves an existing result. JSON retains full
    # precision for arithmetic checks; console values use one decimal place.
    with (args.output_dir / "holtrop_power.json").open("x", encoding="utf-8") as stream:
        json.dump(result, stream, indent=2, allow_nan=False)
        stream.write("\n")
    print("Synthetic screening example; no real-hull numerical validation.")
    for name, force in result["components_n"].items():
        print(f"{name}: {force:.1f} N")
    print(f"total_resistance_n: {result['total_resistance_n']:.1f} N")
    print(f"effective_power_w: {result['effective_power_w']:.1f} W")


if __name__ == "__main__":
    main()
