# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run the synthetic lateral balance in the accompanying documentation.

Basis: docs/domains/pipelines/lateral-balance-example.md, Synthetic inputs
and Hand calculation. Every physical input is an arithmetic assumption, not
a value from a standard. The balance is documented by the existing
``simplified.lateral_stability_check`` API: R = mu * (W - L), D = gamma * H.
No passive soil resistance is credited. This is not DNV qualification.
"""

import json
import math

from digitalmodel.subsea.on_bottom_stability import lateral_stability_check


APPLICABILITY = (
    "Illustrative static lateral force balance only; not DNV qualification. "
    "All inputs must be finite and explicitly supplied, including at zero load: "
    "positive submerged weight, positive friction, nonnegative load magnitudes, "
    "lift below submerged weight, and safety factor at least unity. "
    "No passive soil resistance; no hydrodynamic load derivation, displacement "
    "assessment, cyclic soil response or separate vertical stability assessment."
)


def evaluate_balance(
    *,
    submerged_weight_n_per_m: float,
    horizontal_force_n_per_m: float,
    lift_force_n_per_m: float,
    friction_coefficient: float,
    safety_factor: float,
) -> dict[str, float | bool]:
    """Evaluate the existing lateral API within this example's applicability.

    Args:
        submerged_weight_n_per_m: Positive submerged weight W, in N/m.
        horizontal_force_n_per_m: Nonnegative horizontal load magnitude H, N/m.
        lift_force_n_per_m: Nonnegative upward lift L, N/m, less than W.
        friction_coefficient: Positive caller-supplied friction coefficient mu.
        safety_factor: Caller-supplied demand multiplier gamma, at least unity.

    Returns:
        Resistance and demand in N/m, dimensionless utilization and API verdict.

    Raises:
        ValueError: An input is nonfinite or outside the example's domain.
    """
    inputs = {
        "submerged_weight_n_per_m": submerged_weight_n_per_m,
        "horizontal_force_n_per_m": horizontal_force_n_per_m,
        "lift_force_n_per_m": lift_force_n_per_m,
        "friction_coefficient": friction_coefficient,
        "safety_factor": safety_factor,
    }
    for name, value in inputs.items():
        if not math.isfinite(value):
            raise ValueError(f"{name} must be finite")

    # Domain requirements, not standard-derived coefficients; see Applicability
    # in the accompanying document. Zero denotes absence and unity no factoring.
    if submerged_weight_n_per_m <= 0.0:
        raise ValueError("submerged_weight_n_per_m must be positive")
    if horizontal_force_n_per_m < 0.0:
        raise ValueError("horizontal_force_n_per_m must be a nonnegative magnitude")
    if not 0.0 <= lift_force_n_per_m < submerged_weight_n_per_m:
        raise ValueError("lift_force_n_per_m must be nonnegative and below weight")
    if friction_coefficient <= 0.0:
        raise ValueError("friction_coefficient must be positive")
    if safety_factor < 1.0:
        raise ValueError("safety_factor must be at least unity")

    resistance = friction_coefficient * (
        submerged_weight_n_per_m - lift_force_n_per_m
    )
    demand = safety_factor * horizontal_force_n_per_m
    result = lateral_stability_check(**inputs)
    return {
        "resistance_n_per_m": resistance,
        "demand_n_per_m": demand,
        "utilization": result.utilization,
        "is_stable": result.is_stable,
    }


def run_example() -> dict[str, object]:
    """Return cases on both sides of the boundary and the zero-load limit."""
    # Synthetic inputs and hand calculation in the accompanying document:
    # W = 1200 N/m, L = 200 N/m, mu = 0.6, gamma = 1.2.
    # H = 400 or 600 N/m gives D/R = 0.8 or 1.2. H = L = 0 gives U = 0.
    cases = []
    for name, horizontal, lift in (
        ("accepted", 400.0, 200.0),
        ("rejected", 600.0, 200.0),
        ("zero_load", 0.0, 0.0),
    ):
        inputs = {
            "submerged_weight_n_per_m": 1200.0,
            "horizontal_force_n_per_m": horizontal,
            "lift_force_n_per_m": lift,
            "friction_coefficient": 0.6,
            "safety_factor": 1.2,
        }
        cases.append(
            {"case": name, "inputs": inputs, "result": evaluate_balance(**inputs)}
        )
    return {
        "input_basis": "synthetic assumptions chosen for independent arithmetic",
        "applicability": APPLICABILITY,
        "cases": cases,
    }


if __name__ == "__main__":
    print(json.dumps(run_example(), indent=2, allow_nan=False))
