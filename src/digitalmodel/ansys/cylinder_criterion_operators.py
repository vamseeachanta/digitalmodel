# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline operator mapping for frozen cylinder check receipts.

Limits are read from the receipt, not redefined here. Mapping a receipt does not
authenticate its source, freeze a campaign or establish engineering qualification.
"""

from digitalmodel.infrastructure.analysis_criteria import (
    Criterion,
    CriterionResult,
    Quantity,
)
from digitalmodel.ansys.cylinder_criteria import STATIONS

_BASIS = "docs/plans/evidence/2026-09-13-issue-2121-canary-proposal.json"
_STRESSES = {"sigma_r", "sigma_theta", "sigma_z", "tau_rz", "sigma_vm"}
_DISPLACEMENTS = {"u_r", "u_z"}
_RULES = {
    "accuracy": "accuracy",
    "expected_zero": "expected_zero",
    "refinement_magnitude": "refinement",
    "refinement_decreasing": "refinement",
    "auxiliary": "auxiliary",
    "axial_equilibrium": "axial_equilibrium",
    "zero_control": "zero_control",
}


def _mapping(check: dict) -> tuple[str, str]:
    rule, response = check["criterion"], check["response"]
    if not isinstance(rule, str) or rule not in _RULES:
        raise ValueError("Unmapped cylinder criterion")
    if rule == "axial_equilibrium":
        if response != "RFY":
            raise ValueError("Axial equilibrium requires RFY")
        return "N", _RULES[rule]
    if not isinstance(response, (list, tuple)) or len(response) != 2:
        raise ValueError("Expected a station and quantity pair")
    station, quantity = response
    if not isinstance(station, str) or station not in STATIONS:
        raise ValueError("Expected a frozen response station")
    if not isinstance(quantity, str) or quantity not in _STRESSES | _DISPLACEMENTS:
        raise ValueError("Unmapped cylinder quantity")
    if rule == "expected_zero" and not (
        quantity in {"sigma_z", "tau_rz"}
        or quantity == "sigma_r"
        and station.startswith("outer_")
    ):
        raise ValueError("Quantity is not definition-zero at this station")
    unit = "mm" if quantity in _DISPLACEMENTS else "MPa"
    basis = _RULES[rule]
    if rule == "auxiliary":
        basis = "auxiliary_displacement" if unit == "mm" else "auxiliary_stress"
    return unit, basis


def evaluate_cylinder_check(check: dict) -> CriterionResult:
    """Recompute a residual bound without trusting the receipt's passed flag."""
    if not isinstance(check, dict) or not {
        "criterion",
        "response",
        "limit",
        "residual",
    }.issubset(check):
        raise ValueError("Incomplete cylinder check receipt")
    unit, basis = _mapping(check)
    criterion = Criterion(
        operator="absolute_residual",
        comparator="<=",
        response_unit=unit,
        limit=Quantity(check["limit"], unit),
        source=f"{_BASIS} (2026), criteria: {basis}",
    )
    # Zero is the residual origin in |residual - 0|, not an engineering limit.
    return criterion.evaluate(Quantity(check["residual"], unit), Quantity("0", unit))
