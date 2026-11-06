#!/usr/bin/env python3
# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run a published-input resistance and effective-power bookkeeping example.

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

# Holtrop & Mennen (1982), section 5, p.170, hypothetical numerical example;
# DOI: 10.3233/ISP-1982-2933501. Definitions of Cb and Cp: section 2.
# Knot conversion: NIST SP 811 (2008), section 5.2, table 9.
REFERENCE_INPUTS: dict[str, Any] = {
    "lwl": 205.0,
    "beam": 32.0,
    "draft": 10.0,
    "cb": 37500.0 / (205.0 * 32.0 * 10.0),
    "cm": 0.980,
    "cwp": 0.750,
    "cp": 37500.0 / (205.0 * 32.0 * 10.0 * 0.980),
    "lcb_pct": -2.02,  # Published aft offset; the API takes positive forward.
    "cstern": 10,
    "speed_ms": 25.0 * 1852.0 / 3600.0,
    "abt": 20.0,
    "tf": 10.0,
    "hb": 4.0,
    "at": 16.0,
    "appendage_areas": [50.0],
    "appendage_factors": [1.5],
}


def build_result() -> dict[str, Any]:
    """Return component forces in N and effective power in W without writing."""
    inputs = REFERENCE_INPUTS
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
    # RA = rho*V^2*S*CA/2: Holtrop & Mennen (1982), section 2, p.168.
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
        "input_source": {
            "doi": "10.3233/ISP-1982-2933501",
            "section": "5. Numerical example",
            "page": 170,
            "case_kind": "published hypothetical hull",
        },
        "limitation": "Bookkeeping only; no reproduction or empirical validation.",
        "inputs": inputs,
        # Separate historical assumptions, with external references and the
        # fluid-state limitation given in the usage page; no constants change.
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
    with (args.output_dir / "holtrop_power.json").open(
        "x", encoding="utf-8", newline="\n",
    ) as stream:
        json.dump(result, stream, indent=2, allow_nan=False)
        stream.write("\n")
    print("Published hypothetical inputs; no reproduction or empirical validation.")
    for name, force in result["components_n"].items():
        print(f"{name}: {force:.1f} N")
    print(f"total_resistance_n: {result['total_resistance_n']:.1f} N")
    print(f"effective_power_w: {result['effective_power_w']:.1f} W")


if __name__ == "__main__":
    main()
