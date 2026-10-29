# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run a sourced static-friction balance through the lateral stability API.

Basis: OpenStax, University Physics Volume 1, 2016 edition (May 2018 release),
section 6.2, Eq. 6.1 and Example 6.10. Source forces are divided by one metre
to exercise the N/m API; the source is a crate, not a submerged pipeline.
See docs/domains/pipelines/lateral-balance-example.md for attribution and mapping.
No passive soil resistance is credited. This is not DNV qualification.
"""

import json
import math

from digitalmodel.subsea.on_bottom_stability import lateral_stability_check


APPLICABILITY = (
    "Illustrative static lateral force balance only; not DNV qualification. "
    "Textbook crate forces normalized by one metre; not pipeline design data. "
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
    """Return sourced cases with presentation precision of three decimals."""
    # OpenStax (2016), section 6.2, Eq. 6.1 and Example 6.10, cases (c)/(d):
    # W = 20.0 * 9.80 = 196 N; mu = 0.700; H = 120 or 180 N; L = 0.
    # Divide forces by one metre to use N/m; gamma = 1 means no factoring.
    # The load increase is 180 / 120 = 1.5; R = 0.700 * 196 = 137.2 N/m.
    weight = 20.0 * 9.80 / 1.0
    horizontal = 120.0 / 1.0
    cases = []
    for name, load in (
        ("accepted", horizontal),
        ("rejected", 1.5 * horizontal),
        ("zero_load", 0.0),
    ):
        inputs = {
            "submerged_weight_n_per_m": weight,
            "horizontal_force_n_per_m": load,
            "lift_force_n_per_m": 0.0,
            "friction_coefficient": 0.700,
            "safety_factor": 1.0,
        }
        result = evaluate_balance(**inputs)
        # The API verdict uses the unrounded ratio; rounding is display only.
        result["utilization"] = round(result["utilization"], 3)
        cases.append({"case": name, "inputs": inputs, "result": result})
    return {
        "input_basis": (
            "OpenStax, University Physics Volume 1 (2016), section 6.2, "
            "Example 6.10; forces divided by one metre for the N/m API"
        ),
        "criterion": (
            "U <= 1.000: demand does not exceed static friction capacity; "
            "OpenStax (2016), section 6.2, Eq. 6.1; no design safety factoring"
        ),
        "applicability": APPLICABILITY,
        "cases": cases,
    }


if __name__ == "__main__":
    print(json.dumps(run_example(), indent=2, allow_nan=False))
