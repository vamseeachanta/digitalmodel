# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Opt-in deposition sensitivity using the existing CP demand and mass kernels.

Factors multiply the existing initial, mean and final bare-steel densities
before applying each phase's coating breakdown. The mean factor represents
effective lifetime-average demand reduction, not an endpoint interpolation.
The default is unity (no additional credit). Existing phase densities can
already include deposition effects: the engineer must avoid double counting.
The reference case means no *additional* calcareous reduction, rather than
claiming that the existing density basis describes deposit-free steel.
"""

from collections.abc import Mapping
import math
from typing import Any

from digitalmodel.cathodic_protection import _kernels as kernel


PHASES = ("initial", "mean", "final")
BREAKDOWN_KEYS = ("f_ci", "f_cm", "f_cf")


def _phase_factors(config: Mapping[str, Any]) -> dict[str, float]:
    if set(config) - {"factor", "phase_factors"}:
        raise ValueError("calcareous contains unknown factor input keys")
    if "factor" in config and "phase_factors" in config:
        raise ValueError("calcareous: use factor or phase_factors, not both")
    raw = config.get("phase_factors", {p: config.get("factor", 1.0) for p in PHASES})
    if not isinstance(raw, Mapping) or set(raw) != set(PHASES):
        raise ValueError("phase_factors must contain exactly initial, mean and final")
    result = {}
    for phase in PHASES:
        try:
            value = float(raw[phase])
        except (ValueError, TypeError) as exc:
            raise ValueError("calcareous factor must be finite and in (0, 1]") from exc
        if isinstance(raw[phase], bool) or not math.isfinite(value) or not 0 < value <= 1:
            raise ValueError("calcareous factor must be finite and in (0, 1]")
        result[phase] = value
    return result


def calcareous_demand(
    config: Mapping[str, Any], zone_demands: Mapping[str, Mapping[str, Any]], *,
    design_life_years: float, capacity_Ah_kg: float, utilization_factor: float,
    individual_mass_kg: float, connected_current_A: float = 0.0,
) -> dict[str, Any]:
    """Compare reference and factored phase demands and lifetime net anode mass.

    Connected current allowances retain their reference values. Mass uses mean
    current over the full life; mass count alone is not a current-output check.
    Inputs are the validated offshore route's unrounded densities and breakdown.
    """
    factors = _phase_factors(config)
    totals = {case: {p: connected_current_A for p in PHASES}
              for case in ("without_calcareous", "with_calcareous")}
    zones = {}
    for name, raw in zone_demands.items():
        cases: dict[str, Any] = {}
        effective_factors = factors if raw.get("calcareous_applicable", True) else dict.fromkeys(PHASES, 1.0)
        for case, multiplier in (("without_calcareous", {p: 1.0 for p in PHASES}),
                                 ("with_calcareous", effective_factors)):
            densities = {p: raw[f"i_{p}_A_m2"] * multiplier[p] for p in PHASES}
            demands = {p: kernel.current_demand(raw["area_m2"], densities[p], raw[fc])
                       for p, fc in zip(PHASES, BREAKDOWN_KEYS)}
            for phase in PHASES:
                totals[case][phase] += demands[phase]
            cases[case] = {"densities_A_m2": densities, "demands_A": demands}
        cases["phase_factors"] = effective_factors
        zones[name] = cases
    result: dict[str, Any] = {
        "phase_factors": factors, "zones": zones,
        "design_life_years": design_life_years,
        "connected_current_A": connected_current_A,
        "basis": "User sensitivity factors on existing phase densities; mean is lifetime-effective",
    }
    for case, demands in totals.items():
        mass = kernel.anode_mass(demands["mean"], design_life_years,
                                 capacity_Ah_kg, utilization_factor)
        result[case] = {"demands_A": demands, "total_mass_kg": mass,
                        "count_by_mass": kernel.anode_count(mass, individual_mass_kg)}
    return result
