# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline mapping tests; no native solver or historical evidence is accessed."""

from decimal import Decimal
from fractions import Fraction

import pytest

from digitalmodel.ansys.cylinder_criterion_operators import evaluate_cylinder_check
from digitalmodel.ansys.cylinder_criteria import evaluate_canary
from tests.ansys.test_cylinder_criteria import attempts, reference


@pytest.mark.parametrize("residual", ["-2", "2"])
def test_adapter_recomputes_magnitude_instead_of_trusting_passed_flag(residual):
    """Synthetic hand calculation: |-2-0| = |2-0| = 2 > 1 MPa.

    DLMF 1.2.8 (2026), eq. 3.1.8. This is a synthetic check receipt, not
    an amendment to the frozen cylinder limits; its passed flag is untrusted.
    """
    result = evaluate_cylinder_check(
        {
            "criterion": "zero_control",
            "response": ["inner_y120", "sigma_r"],
            "residual": residual,
            "limit": "1",
            "passed": True,
        }
    )
    assert result.status == "FAIL"
    assert result.metric == Fraction(2)
    assert result.unit == "MPa"
    assert result.criterion.operator == "absolute_residual"
    assert result.criterion.comparator == "<="


@pytest.mark.parametrize(
    "response,rule,unit,basis",
    [
        (["inner_y120", "sigma_theta"], "accuracy", "MPa", "accuracy"),
        (["inner_y120", "u_r"], "accuracy", "mm", "accuracy"),
        (["inner_y120", "sigma_z"], "expected_zero", "MPa", "expected_zero"),
        (["inner_y60", "sigma_r"], "auxiliary", "MPa", "auxiliary_stress"),
        (["inner_y60", "u_z"], "auxiliary", "mm", "auxiliary_displacement"),
        ("RFY", "axial_equilibrium", "N", "axial_equilibrium"),
        (["inner_y120", "u_r"], "refinement_magnitude", "mm", "refinement"),
        (["inner_y120", "sigma_r"], "refinement_decreasing", "MPa", "refinement"),
    ],
)
def test_mapping_keeps_response_units_limit_and_original_basis(
    response, rule, unit, basis
):
    """Hand comparison: |1-0| <= 1, inclusive in each declared response unit.

    Synthetic values isolate metadata mapping. Rule identities come from the
    retained 2026 canary proposal, criteria entries; no limit is supplied by it.
    """
    result = evaluate_cylinder_check(
        {
            "criterion": rule,
            "response": response,
            "residual": "1",
            "limit": "1",
        }
    )
    assert result.status == "PASS"
    assert result.unit == unit
    assert result.criterion.limit.value == Decimal("1")
    assert result.criterion.source.endswith(f"criteria: {basis}")


@pytest.mark.parametrize(
    "changes",
    [
        {"criterion": "unknown"},
        {"response": ["inner_y120", "unknown"]},
        {"response": "RFY"},
        {"response": []},
        {"limit": "NaN"},
        {"limit": "-1"},
        {"residual": 1.0},
        {"response": ["unmapped_station", "sigma_r"]},
        {"criterion": "expected_zero", "response": ["inner_y120", "u_r"]},
    ],
)
def test_adapter_refuses_unmapped_or_invalid_receipts(changes):
    """Malformed metadata must not default to stress units or a known operator."""
    check = {
        "criterion": "accuracy",
        "response": ["inner_y120", "sigma_r"],
        "residual": "1",
        "limit": "1",
    }
    with pytest.raises(ValueError):
        evaluate_cylinder_check(check | changes)


def test_all_frozen_cylinder_checks_have_explicit_equivalent_operators():
    """Cross-implementation parity, not an independent cylinder solution.

    The existing analytical synthetic fixture and frozen evaluator supply
    legacy receipts. Every receipt is re-evaluated with the reusable operator;
    independent hand calculations for the metric are tested separately above.
    """
    legacy = evaluate_canary(attempts(), reference())
    assert legacy["status"] == "PASS"
    rules = set()
    for check in legacy["checks"]:
        result = evaluate_cylinder_check(check)
        assert (result.status == "PASS") == check["passed"]
        assert result.criterion.operator == "absolute_residual"
        assert result.criterion.comparator == "<="
        rules.add(check["criterion"])
    assert rules == {
        "zero_control",
        "axial_equilibrium",
        "accuracy",
        "expected_zero",
        "auxiliary",
        "refinement_magnitude",
        "refinement_decreasing",
    }
