# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic cashflows; all amounts, rates and years are test assumptions.

Independent arithmetic below is the numerical basis, without a finance API.
Negative, nonfinite and incorrectly typed values are validation probes.
"""

from copy import deepcopy

import pytest
from pydantic import ValidationError

from digitalmodel.cathodic_protection.cp_comparison import (
    ComparisonInput,
    ComparisonResult,
    compare_systems,
)


@pytest.fixture
def cashflows():
    """Two already-sized synthetic alternatives; amounts are arbitrary USD."""
    return {
        "design_life_years": 2,
        "discount_rate": 0.1,
        "provenance": "Synthetic common financial basis; no escalation or salvage.",
        "iccp": {
            "currency": "USD",
            "initial_cost": 1000,
            "annual_costs": [
                {"year": 1, "operating_cost": 110, "replacement_cost": 0},
                {"year": 2, "operating_cost": 121, "replacement_cost": 242},
            ],
            "provenance": "Synthetic sized ICCP and assumed cost schedule.",
        },
        "sacp": {
            "currency": "USD",
            "initial_cost": 1200,
            "annual_costs": [
                {"year": 1, "operating_cost": 22, "replacement_cost": 0},
                {"year": 2, "operating_cost": 24.2, "replacement_cost": 121},
            ],
            "provenance": "Synthetic sized SACP and assumed cost schedule.",
        },
    }


def test_zero_discount_equals_cash_sum(cashflows):
    """ICCP: 1000+110+121+242=1473; SACP: 1200+22+24.2+121=1367.2."""
    cashflows["discount_rate"] = 0
    result = compare_systems(ComparisonInput.model_validate(cashflows))
    assert result.iccp.undiscounted.initial_cost == 1000
    assert result.iccp.undiscounted.operating_cost == 231
    assert result.iccp.undiscounted.replacement_cost == 242
    assert result.iccp.undiscounted.total_cost == 1473
    assert result.iccp.npv.total_cost == 1473
    assert result.sacp.npv.total_cost == pytest.approx(1367.2)
    # 1473 - 1367.2 = 105.8; positive means ICCP costs more.
    assert result.npv_difference_iccp_minus_sacp == pytest.approx(105.8)
    assert result.iccp.undiscounted == result.iccp.npv
    assert result.sacp.undiscounted == result.sacp.npv


def test_discounted_two_year_costs(cashflows):
    """End-year: 1.1 and 1.1^2=1.21 are independent discount divisors.

    ICCP: 1000 + 110/1.1 + (121+242)/1.21 = 1400.
    SACP: 1200 + 22/1.1 + (24.2+121)/1.21 = 1340.
    """
    result = compare_systems(ComparisonInput.model_validate(cashflows))
    assert result.iccp.npv.initial_cost == 1000
    assert result.iccp.npv.operating_cost == pytest.approx(200)
    assert result.iccp.npv.replacement_cost == pytest.approx(200)
    assert result.iccp.npv.total_cost == pytest.approx(1400)
    assert result.sacp.npv.operating_cost == pytest.approx(40)
    assert result.sacp.npv.replacement_cost == pytest.approx(100)
    assert result.sacp.npv.total_cost == pytest.approx(1340)
    assert result.npv_difference_iccp_minus_sacp == pytest.approx(60)


def test_replacement_timing(cashflows):
    """Moving 242 from year two to one changes PV from 200 to 220."""
    original = compare_systems(ComparisonInput.model_validate(cashflows))
    cashflows["iccp"]["annual_costs"][0]["replacement_cost"] = 242
    cashflows["iccp"]["annual_costs"][1]["replacement_cost"] = 0
    earlier = compare_systems(ComparisonInput.model_validate(cashflows))
    assert earlier.iccp.npv.replacement_cost == pytest.approx(220)
    assert earlier.iccp.npv.total_cost - original.iccp.npv.total_cost == pytest.approx(20)
    assert earlier.iccp.undiscounted == original.iccp.undiscounted


def test_identical_alternatives(cashflows):
    cashflows["sacp"] = deepcopy(cashflows["iccp"])
    result = compare_systems(ComparisonInput.model_validate(cashflows))
    assert result.iccp == result.sacp
    assert result.npv_difference_iccp_minus_sacp == 0


def test_difference_can_be_negative(cashflows):
    cashflows["iccp"], cashflows["sacp"] = cashflows["sacp"], cashflows["iccp"]
    result = compare_systems(ComparisonInput.model_validate(cashflows))
    assert result.npv_difference_iccp_minus_sacp == pytest.approx(-60)


def test_input_basis_and_json_round_trip(cashflows):
    basis = ComparisonInput.model_validate(cashflows)
    before = basis.model_dump()
    result = compare_systems(basis)
    assert isinstance(result, ComparisonResult)
    assert result.input_basis == basis
    assert result.currency == "USD"
    assert result.discount_timing == "end_of_year"
    assert basis.model_dump() == before
    assert ComparisonResult.model_validate_json(result.model_dump_json()) == result


def test_schedule_order_does_not_change_result(cashflows):
    expected = compare_systems(ComparisonInput.model_validate(cashflows))
    cashflows["iccp"]["annual_costs"].reverse()
    actual = compare_systems(ComparisonInput.model_validate(cashflows))
    assert actual.iccp == expected.iccp


def test_one_year_and_explicit_zero_costs(cashflows):
    """Synthetic boundary: one year, zero capital and zero annual costs."""
    cashflows["design_life_years"] = 1
    for system in ("iccp", "sacp"):
        cashflows[system]["initial_cost"] = 0
        cashflows[system]["annual_costs"] = [
            {"year": 1, "operating_cost": 0, "replacement_cost": 0}
        ]
    result = compare_systems(ComparisonInput.model_validate(cashflows))
    assert result.iccp.npv.total_cost == result.sacp.npv.total_cost == 0


@pytest.mark.parametrize("system", ["iccp", "sacp"])
@pytest.mark.parametrize("schedule", [
    [],
    [{"year": 1, "operating_cost": 0, "replacement_cost": 0}],
    [{"year": 1, "operating_cost": 0, "replacement_cost": 0}] * 2,
    [{"year": 0, "operating_cost": 0, "replacement_cost": 0},
     {"year": 2, "operating_cost": 0, "replacement_cost": 0}],
    [{"year": 1, "operating_cost": 0, "replacement_cost": 0},
     {"year": 3, "operating_cost": 0, "replacement_cost": 0}],
    [{"year": 1, "operating_cost": 0, "replacement_cost": 0},
     {"year": 2, "operating_cost": 0, "replacement_cost": 0},
     {"year": 3, "operating_cost": 0, "replacement_cost": 0}],
])
def test_invalid_schedules(cashflows, system, schedule):
    cashflows[system]["annual_costs"] = schedule
    with pytest.raises(ValidationError):
        ComparisonInput.model_validate(cashflows)


@pytest.mark.parametrize("currency", ["EUR", "usd", "", "US", "USDD", True])
def test_invalid_or_mismatched_currency(cashflows, currency):
    cashflows["sacp"]["currency"] = currency
    with pytest.raises(ValidationError):
        ComparisonInput.model_validate(cashflows)


@pytest.mark.parametrize("path", [
    ("design_life_years",), ("discount_rate",), ("iccp", "initial_cost"),
    ("sacp", "initial_cost"), ("iccp", "annual_costs", 0, "year"),
    ("iccp", "annual_costs", 0, "operating_cost"),
    ("sacp", "annual_costs", 1, "replacement_cost"),
])
@pytest.mark.parametrize("value", [True, False, float("nan"), float("inf"),
                                   float("-inf"), -1, "1", None])
def test_invalid_numeric_inputs(cashflows, path, value):
    target = cashflows
    for key in path[:-1]:
        target = target[key]
    target[path[-1]] = value
    with pytest.raises(ValidationError):
        ComparisonInput.model_validate(cashflows)


@pytest.mark.parametrize("field,value", [("design_life_years", 0),
                                         ("design_life_years", 1.5),
                                         ("design_life_years", 2.0)])
def test_invalid_design_life(cashflows, field, value):
    cashflows[field] = value
    with pytest.raises(ValidationError):
        ComparisonInput.model_validate(cashflows)


@pytest.mark.parametrize("value", [1.0, 1.5, "1"])
def test_year_requires_integer(cashflows, value):
    cashflows["iccp"]["annual_costs"][0]["year"] = value
    with pytest.raises(ValidationError):
        ComparisonInput.model_validate(cashflows)


@pytest.mark.parametrize("path", [
    ("design_life_years",), ("discount_rate",), ("provenance",),
    ("iccp", "currency"), ("iccp", "initial_cost"),
    ("iccp", "annual_costs"), ("iccp", "provenance"),
    ("sacp", "provenance"), ("sacp", "annual_costs", 0, "year"),
    ("sacp", "annual_costs", 0, "operating_cost"),
    ("sacp", "annual_costs", 0, "replacement_cost"),
])
def test_required_inputs_have_no_defaults(cashflows, path):
    target = cashflows
    for key in path[:-1]:
        target = target[key]
    del target[path[-1]]
    with pytest.raises(ValidationError):
        ComparisonInput.model_validate(cashflows)


@pytest.mark.parametrize("system", [None, "iccp", "sacp"])
@pytest.mark.parametrize("value", ["", "  ", True])
def test_provenance_is_required_text(cashflows, system, value):
    target = cashflows if system is None else cashflows[system]
    target["provenance"] = value
    with pytest.raises(ValidationError):
        ComparisonInput.model_validate(cashflows)


def test_unknown_fields_rejected(cashflows):
    cashflows["iccp"]["annual_costs"][0]["operatng_cost"] = 10
    with pytest.raises(ValidationError):
        ComparisonInput.model_validate(cashflows)


def test_constructed_invalid_model_revalidated(cashflows):
    basis = ComparisonInput.model_validate(cashflows)
    invalid = basis.model_copy(update={"discount_rate": True})
    with pytest.raises(ValidationError):
        compare_systems(invalid)


def test_overflow_rejected(cashflows):
    """Finite synthetic probes whose undiscounted sum exceeds float range."""
    cashflows["iccp"]["initial_cost"] = 1e308
    cashflows["iccp"]["annual_costs"][0]["operating_cost"] = 1e308
    with pytest.raises(ValueError, match="finite|range|overflow"):
        compare_systems(ComparisonInput.model_validate(cashflows))


def test_large_rate_does_not_prematurely_underflow(cashflows):
    """Synthetic limit: 1e308 / (1e308)^2 = 1e-308, still representable.

    At this scale floating-point 1+r equals r. Computing the discount
    factor first would incorrectly underflow to zero before multiplication.
    """
    cashflows["discount_rate"] = 1e308
    cashflows["iccp"]["initial_cost"] = 0
    cashflows["iccp"]["annual_costs"] = [
        {"year": 1, "operating_cost": 0, "replacement_cost": 0},
        {"year": 2, "operating_cost": 0, "replacement_cost": 1e308},
    ]
    result = compare_systems(ComparisonInput.model_validate(cashflows))
    assert result.iccp.npv.total_cost == pytest.approx(1e-308, rel=1e-12, abs=0)
