"""Synthetic 40 m tank example; run with PYTHONPATH=src uv run python <file>.

All ratings, resistances, limits and potentials below are demonstration
inputs, not measured data or API defaults. The 0.047 A/m ribbon rating is
chosen to demonstrate the 668.4 m arithmetic and requires a qualified basis
at the actual electrolyte and intended design life before engineering use.
"""

from __future__ import annotations

import json

from digitalmodel.cathodic_protection import (
    TankBottomInput, assess_tank_bottom, check_center_edge_polarization,
)


def worked_example() -> dict:
    """Return inputs, independent sizing checks and synthetic local potentials."""
    inputs = TankBottomInput(
        diameter_m=40.0,
        current_density_mA_m2=25.0,
        ribbon_output_A_m=0.047,
        ribbon_spacing_m=1.8,
        max_spacing_m=2.0,
        ribbon_resistance_ohm_m=0.1,
        ribbon_feed_span_m=8.0,
        busbar_resistance_ohm_m=0.01,
        busbar_feed_length_m=20.0,
        busbar_current_A=10.0,
        foundation_resistivity_ohm_m=100.0,
        max_foundation_resistivity_ohm_m=200.0,
    )
    sizing = assess_tank_bottom(inputs)
    potentials = check_center_edge_polarization(
        -0.90, -0.95, criterion_V=-0.85,
    )
    return {
        "data_status": "synthetic demonstration; not engineering-qualified",
        "inputs": inputs.model_dump(mode="json"),
        "sizing": sizing.model_dump(mode="json"),
        "synthetic_potential_check": potentials.model_dump(mode="json"),
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2))
