# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Select available production models without substituting deprecated aliases.

The policy and its source limitations are documented in
docs/domains/production/production-engineering-method-selection.md.
Selection does not fit model parameters or establish numerical validation.
Assessment sources: Vogel (1968), doi:10.2118/1476-PA, abstract;
Fetkovich (1973), doi:10.2118/4529-MS, abstract; Standing (1970),
doi:10.2118/3237-PA, introduction. Equation locators: Nass (2010),
Texas A&M MS thesis, Eqs. (1.7), (1.12), (1.16). Full links and access
limits are recorded in the literature document. No design limits are inferred.
"""

from __future__ import annotations

import math
from collections.abc import Mapping
from dataclasses import dataclass
from typing import Literal


@dataclass(frozen=True)
class SelectionAssessment:
    """A screening decision with explicit evidence gaps, never design approval.

    ``method=None`` means no available method was selected. Even a selected
    method has ``qualification_status='NOT_EVALUATED'``: the selector has no
    numerical applicability, calibration or original-example evidence.
    """

    method: str | None
    rationale: str
    sources: tuple[str, ...]
    open_checks: tuple[str, ...]
    qualification_status: Literal["NOT_EVALUATED"] = "NOT_EVALUATED"


_VOGEL = "Vogel (1968), doi:10.2118/1476-PA, abstract and introduction"
_FETKOVICH = "Fetkovich (1973), doi:10.2118/4529-MS, abstract"
_STANDING = "Standing (1970), doi:10.2118/3237-PA, introduction"
_IPR_BASIS = {
    "linear": (
        "The requested pressure interval stays at or above bubble point.",
        (_VOGEL, "Nass (2010), MS thesis, Eq. (1.7)"),
    ),
    "vogel": (
        "Confirmed solution-gas drive with reservoir at or below bubble point.",
        (_VOGEL, _STANDING, "Nass (2010), MS thesis, Eq. (1.12)"),
    ),
    "composite": (
        "Reservoir above bubble point; the requested interval crosses below it.",
        (
            _VOGEL,
            "production-engineering-literature.md, Composite join derivation",
        ),
    ),
    "fetkovich": (
        "Multirate data are declared available; fitted parameters remain required.",
        (_FETKOVICH, "Nass (2010), MS thesis, Eq. (1.16)"),
    ),
}
_IPR_OPEN = (
    "Numerical applicability envelope: NOT_EVALUATED.",
    "Parameter fit and field calibration: NOT_EVALUATED.",
    "Original-paper worked-example validation: NOT_EVALUATED.",
)
_VLP_INVENTORY = (
    "vlp_correlations.py, simplified_homogeneous_pwf and deprecated aliases",
    "production-engineering-method-selection.md, VLP comparison and policy",
)
_VLP_OPEN = (
    "Published-correlation implementation and numerical ranges: NOT_EVALUATED.",
    "Measured pressure-survey calibration: NOT_EVALUATED.",
    "Original-paper worked-example validation: NOT_EVALUATED.",
)


def _pressure(state: Mapping[str, object], key: str) -> float:
    value = state.get(key)
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{key} must be a finite pressure in psi")
    # Zero is the absolute-pressure domain boundary, not a design limit;
    # Nass (2010), Eqs. (1.5)-(1.9), including the zero-pressure endpoint.
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
    # Positive reference pressures and nonnegative drawdown define the IPR
    # domain: Nass (2010), Eqs. (1.5), (1.12), (1.16); no acceptance band.
    if pr == 0 or pb == 0 or pwf > pr:
        raise ValueError(
            "Require positive reservoir/bubble pressures and 0 <= Pwf <= Pr"
        )
    if fluid_props.get("phase") != "oil" or _flag(fluid_props, "significant_skin"):
        raise NotImplementedError("No qualified IPR selection for this fluid or skin")
    if multirate:
        return "fetkovich"
    if pwf >= pb:
        return "linear"
    if not solution_gas:
        raise NotImplementedError(
            "Below-bubble-point selection requires solution-gas drive"
        )
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


def assess_ipr_selection(
    reservoir_state: Mapping[str, object], fluid_props: Mapping[str, object]
) -> SelectionAssessment:
    """Explain the existing IPR decision and retain unevaluated qualification.

    Unsupported physical cases return no method, with the selector's reason.
    Malformed inputs still raise ValueError, exactly as select_ipr_model does.
    A declared multirate test does not establish its quality or fitted values.
    """
    try:
        method = select_ipr_model(reservoir_state, fluid_props)
    except NotImplementedError as error:
        return SelectionAssessment(
            method=None,
            rationale=str(error),
            sources=(_VOGEL, _FETKOVICH, _STANDING),
            open_checks=_IPR_OPEN,
        )
    rationale, sources = _IPR_BASIS[method]
    return SelectionAssessment(method, rationale, sources, _IPR_OPEN)


def assess_vlp_selection(
    regime: str, fluid_props: Mapping[str, object]
) -> SelectionAssessment:
    """Explain VLP availability without silently selecting a literature candidate.

    An unsupported physical request returns no method. An unknown regime
    still raises ValueError. The explicit screening result retains its lack
    of qualification; deprecated aliases are never returned.
    """
    try:
        method = select_vlp_correlation(regime, fluid_props)
    except NotImplementedError as error:
        return SelectionAssessment(
            method=None,
            rationale=str(error),
            sources=_VLP_INVENTORY,
            open_checks=_VLP_OPEN,
        )
    return SelectionAssessment(
        method=method,
        rationale="Explicit oil screening request; fixed-slip exploratory traverse only.",
        sources=_VLP_INVENTORY,
        open_checks=_VLP_OPEN,
    )
