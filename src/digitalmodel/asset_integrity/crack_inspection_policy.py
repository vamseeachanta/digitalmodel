# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
# ABOUTME: Explicit diagnostic inspection policy from shared crack-growth evidence.
# ABOUTME: Caller-supplied cycle rate, divisor, cap and engineering bases.
"""Apply a diagnostic inspection policy to supplied crack-growth evidence."""

from __future__ import annotations

import json
from typing import Any


def plan_crack_inspection(
    table: list | tuple,
    law: dict,
    *,
    cycles_per_year: float,
    interval_divisor: float,
    policy_cap_years: float,
    policy_basis: str,
    target_depth_basis: str,
) -> dict[str, Any]:
    """Apply an explicit diagnostic policy to shared crack-growth evidence.

    Depths are metres, DeltaK is MPa sqrt(m), life is cycles and time is years.
    The last supplied depth is an assessment target, not a calculated critical
    depth. The supplied cycle rate is constant over the predicted life.
    Neither the growth law nor the policy is qualified for service by this API.
    Null target cycles require review; finite endpoint arrival remains usable
    even when the shared integrator reports arrest at that endpoint.
    """
    from copy import deepcopy

    from digitalmodel.asset_integrity.assessment.fe_crack_growth import (
        basis,
        growth_life,
        scalar,
    )

    cycles_per_year = scalar(cycles_per_year, "cycles_per_year", positive=True)
    interval_divisor = scalar(interval_divisor, "interval_divisor", positive=True)
    if interval_divisor < 1:
        raise ValueError("interval_divisor must be at least one")
    policy_cap_years = scalar(policy_cap_years, "policy_cap_years", positive=True)
    basis(policy_basis, "policy_basis")
    basis(target_depth_basis, "target_depth_basis")
    inputs: dict[str, Any] = deepcopy(
        dict(
            table=table,
            law=law,
            cycles_per_year=cycles_per_year,
            interval_divisor=interval_divisor,
            policy_cap_years=policy_cap_years,
            policy_basis=policy_basis,
            target_depth_basis=target_depth_basis,
        )
    )
    # growth_life validates table shape and ordering before target-depth access.
    growth = growth_life(inputs["table"], inputs["law"])
    years = interval = None
    status = "REVIEW_REQUIRED"
    if growth["cycles"] is not None:
        years = scalar(
            growth["cycles"] / cycles_per_year, "remaining_life_years", positive=True
        )
        uncapped = scalar(
            years / interval_divisor, "policy interval years", positive=True
        )
        interval = min(uncapped, policy_cap_years)
        status = "DIAGNOSTIC_POLICY_ONLY"
    result = dict(
        status=status,
        qualified_engineering_interval=False,
        target_depth_m=inputs["table"][-1][0],
        remaining_life_years=years,
        next_inspection_interval_years=interval,
        growth=growth,
        inputs=inputs,
    )
    # Reject overflow in retained diagnostics as well as the policy arithmetic.
    json.dumps(result, allow_nan=False)
    return result
