# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Select available production models without substituting deprecated aliases.

The policy and its source limitations are documented in
docs/domains/production/production-engineering-method-selection.md.
Selection does not fit model parameters or establish numerical validation.
"""

from __future__ import annotations

import math
from collections.abc import Mapping


def _pressure(state: Mapping[str, object], key: str) -> float:
    value = state.get(key)
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{key} must be a finite pressure in psi")
    if not math.isfinite(value) or value < 0:
        raise ValueError(f"{key} must be a finite nonnegative pressure in psi")
    return float(value)


def _flag(properties: Mapping[str, object], key: str) -> bool:
    value = properties.get(key, False)
    if not isinstance(value, bool):
        raise ValueError(f"{key} must be a boolean")
    return value


def select_ipr_model(
    reservoir_state: Mapping[str, object], fluid_props: Mapping[str, object]
) -> str:
    """Return an existing IPR model name for an oil-well pressure interval.

    Args:
        reservoir_state: Reservoir and bubble-point pressures and minimum
            flowing pressure, using keys reservoir_pressure_psi,
            bubble_point_psi and minimum_pwf_psi. The optional boolean
            multirate_test_available requests a fitted Fetkovich model.
        fluid_props: Required phase='oil'; optional booleans solution_gas_drive
            and significant_skin. Below-bubble-point models require confirmed
            solution-gas drive. Significant skin is outside this policy.

    Returns:
        One of linear, vogel, composite or fetkovich. Constructor parameters
        remain the caller's responsibility, including fitted test coefficients.

    Raises:
        ValueError: A required pressure or boolean is invalid.
        NotImplementedError: Fluid or reservoir behavior is unsupported.
    """
    pr = _pressure(reservoir_state, "reservoir_pressure_psi")
    pb = _pressure(reservoir_state, "bubble_point_psi")
    pwf = _pressure(reservoir_state, "minimum_pwf_psi")
    multirate = _flag(reservoir_state, "multirate_test_available")
    solution_gas = _flag(fluid_props, "solution_gas_drive")
    if pr == 0 or pb == 0 or pwf > pr:
        raise ValueError("Require positive reservoir/bubble pressures and 0 <= Pwf <= Pr")
    if fluid_props.get("phase") != "oil" or _flag(fluid_props, "significant_skin"):
        raise NotImplementedError("No qualified IPR selection for this fluid or skin")
    if multirate:
        return "fetkovich"
    if pwf >= pb:
        return "linear"
    if not solution_gas:
        raise NotImplementedError("Below-bubble-point selection requires solution-gas drive")
    return "composite" if pr > pb else "vogel"


def select_vlp_correlation(regime: str, fluid_props: Mapping[str, object]) -> str:
    """Return the simplified traverse only for an explicit oil screening request.

    Args:
        regime: screening, vertical_oil, inclined_oil or gas_condensate.
        fluid_props: Mapping containing phase; only oil screening is supported.

    Returns:
        simplified_homogeneous, an existing exploratory traverse.

    Raises:
        ValueError: The regime is unknown.
        NotImplementedError: A published correlation or gas model is requested.
    """
    if regime not in {"screening", "vertical_oil", "inclined_oil", "gas_condensate"}:
        raise ValueError(f"Unknown VLP regime: {regime}")
    if regime != "screening" or fluid_props.get("phase") != "oil":
        raise NotImplementedError(
            "Published VLP correlations are not implemented; no alias is substituted"
        )
    return "simplified_homogeneous"
