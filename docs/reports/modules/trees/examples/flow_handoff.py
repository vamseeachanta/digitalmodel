# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Print a synthetic separate-port handoff as strict JSON to stdout only.

All dimensional constants are declared synthetic inputs in the adjacent
README, section "Synthetic inputs". Their hand calculation is documented
there; the coefficient is not a standard value or an equipment design basis.
Importing this module performs no calculation and writes no output.
"""

from __future__ import annotations

from dataclasses import asdict
import json

from digitalmodel.subsea.trees.flow_capacity import bore_capacity, flow_handoff


# Source: README.md, "Synthetic inputs"; SI units, arithmetic fixture only.
PRODUCTION_DIAMETER_M = 0.25
ANNULUS_DIAMETER_M = 0.125
DENSITY_KG_M3 = 400.0
EROSIONAL_COEFFICIENT_SI = 60.0
PRODUCTION_RATE_M3_S = 0.1
ANNULUS_RATE_M3_S = 0.02
UPSTREAM_PRESSURE_PA = 2_000_000.0
PRESSURE_DROP_PA = 250_000.0


def build_example(
    production_rate_m3_s: float = PRODUCTION_RATE_M3_S,
    annulus_rate_m3_s: float = ANNULUS_RATE_M3_S,
) -> dict[str, dict[str, float | bool]]:
    """Calculate both capacities and preserve the supplied separate-port rates."""
    production = bore_capacity(
        PRODUCTION_DIAMETER_M, DENSITY_KG_M3, EROSIONAL_COEFFICIENT_SI
    )
    annulus = bore_capacity(
        ANNULUS_DIAMETER_M, DENSITY_KG_M3, EROSIONAL_COEFFICIENT_SI
    )
    handoff = flow_handoff(
        production_rate_m3_s,
        annulus_rate_m3_s,
        production,
        annulus,
        UPSTREAM_PRESSURE_PA,
        PRESSURE_DROP_PA,
    )
    return {
        "production_capacity": asdict(production),
        "annulus_capacity": asdict(annulus),
        "handoff": asdict(handoff),
    }


def main() -> None:
    """Emit deterministic JSON; reject non-finite numbers and create no files."""
    print(json.dumps(build_example(), allow_nan=False, sort_keys=True, indent=2))


if __name__ == "__main__":
    main()
