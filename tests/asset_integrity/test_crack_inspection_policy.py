# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic crack policy arithmetic; no service interval is qualified."""

from copy import deepcopy
import json

import pytest

from digitalmodel.asset_integrity import inspection_planning as planning
from digitalmodel.asset_integrity.assessment.fe_crack_growth import growth_life


@pytest.fixture
def inputs():
    return dict(
        table=[[0.001, 5.0], [0.002, 2.0]],
        law=dict(
            c=1e-10,
            m=2,
            threshold=2,
            e_ratio=1,
            threshold_rule="unchanged",
            basis="Synthetic growth law",
        ),
        cycles_per_year=1000000,
        interval_divisor=2,
        policy_cap_years=10,
        policy_basis="Synthetic divisor and cap; not a service policy",
        target_depth_basis="Supplied assessment target; not a critical depth",
    )


def test_positive_threshold_endpoint_arrival(inputs):
    # Integral (1/2 - 1/5)/(C*3000) = 1000000 cycles.
    result = planning.plan_crack_inspection(**inputs)
    assert result["growth"] == growth_life(inputs["table"], inputs["law"])
    assert result["growth"]["status"] == "ARRESTED"
    assert result["growth"]["cycles"] == pytest.approx(1000000)
    assert result["remaining_life_years"] == pytest.approx(1)
    assert result["next_inspection_interval_years"] == pytest.approx(0.5)
    assert result["status"] == "DIAGNOSTIC_POLICY_ONLY"
    assert result["qualified_engineering_interval"] is False
    assert result["target_depth_m"] == 0.002
    assert result["inputs"] == inputs
    assert json.loads(json.dumps(result, allow_nan=False)) == result


@pytest.mark.parametrize("cap, expected", [(10, 0.2), (0.1, 0.1)])
def test_constant_propagation_and_cap(inputs, cap, expected):
    # N = da/(C*K**m) = 400000 cycles; life/divisor = 0.2 years.
    inputs["table"] = [[0.001, 5.0], [0.002, 5.0]]
    inputs["policy_cap_years"] = cap
    result = planning.plan_crack_inspection(**inputs)
    assert result["growth"]["status"] == "FINITE"
    assert result["growth"]["cycles"] == pytest.approx(400000)
    assert result["remaining_life_years"] == pytest.approx(0.4)
    assert result["next_inspection_interval_years"] == pytest.approx(expected)


def test_cap_comparison_uses_unrounded_life(inputs):
    inputs["policy_cap_years"] = 0.499999999
    assert (
        planning.plan_crack_inspection(**inputs)["next_inspection_interval_years"]
        == inputs["policy_cap_years"]
    )


@pytest.mark.parametrize(
    "table, threshold, arrest_depth, arrest_cycles",
    [
        ([[0.001, 5], [0.002, 1]], 2, 0.00175, 750000),
        ([[0.001, 2], [0.002, 5]], 2, 0.001, 0),
        ([[0.001, 5], [0.002, 0]], 0, 0.002, None),
        ([[0.001, 5], [0.002, 2], [0.003, 5]], 2, 0.002, 1000000),
    ],
)
def test_unknown_target_life_requires_review(
    inputs,
    table,
    threshold,
    arrest_depth,
    arrest_cycles,
):
    inputs["table"] = table
    inputs["law"]["threshold"] = threshold
    result = planning.plan_crack_inspection(**inputs)
    assert result["growth"] == growth_life(table, inputs["law"])
    assert result["growth"]["cycles"] is None
    assert result["growth"]["arrest_a_m"] == pytest.approx(arrest_depth)
    if arrest_cycles is None:
        assert result["growth"]["cycles_to_arrest"] is None
    else:
        assert result["growth"]["cycles_to_arrest"] == pytest.approx(arrest_cycles)
    assert result["remaining_life_years"] is None
    assert result["next_inspection_interval_years"] is None
    assert result["status"] == "REVIEW_REQUIRED"
    assert result["qualified_engineering_interval"] is False
    json.dumps(result, allow_nan=False)


@pytest.mark.parametrize(
    "field",
    [
        "cycles_per_year",
        "interval_divisor",
        "policy_cap_years",
    ],
)
@pytest.mark.parametrize(
    "value", [0, -1, True, "2", None, float("nan"), float("inf"), 10**400]
)
def test_invalid_policy_numbers(inputs, field, value):
    inputs[field] = value
    with pytest.raises(ValueError, match=field):
        planning.plan_crack_inspection(**inputs)


def test_divisor_below_one(inputs):
    inputs["interval_divisor"] = 0.999
    with pytest.raises(ValueError, match="interval_divisor"):
        planning.plan_crack_inspection(**inputs)


@pytest.mark.parametrize("field", ["policy_basis", "target_depth_basis"])
@pytest.mark.parametrize("value", ["", " \n", None, 2])
def test_missing_basis(inputs, field, value):
    inputs[field] = value
    with pytest.raises(ValueError, match=field):
        planning.plan_crack_inspection(**inputs)


@pytest.mark.parametrize(
    "field",
    [
        "cycles_per_year",
        "interval_divisor",
        "policy_cap_years",
        "policy_basis",
        "target_depth_basis",
    ],
)
def test_policy_has_no_defaults(inputs, field):
    del inputs[field]
    with pytest.raises(TypeError, match=field):
        planning.plan_crack_inspection(**inputs)


def test_nonfinite_life_is_rejected_before_cap(inputs):
    inputs["cycles_per_year"] = 1e-320
    with pytest.raises(ValueError, match="remaining_life_years"):
        planning.plan_crack_inspection(**inputs)


def test_nonfinite_retained_diagnostic_is_rejected(inputs):
    inputs["law"]["threshold"] = 1e-320  # Finite input, overflowing K/threshold.
    with pytest.raises(ValueError, match="finite|range"):
        planning.plan_crack_inspection(**inputs)


def test_evidence_is_independent_of_later_input_mutation(inputs):
    saved = deepcopy(inputs)
    result = planning.plan_crack_inspection(**inputs)
    inputs["table"][0][1] = 99
    inputs["law"]["basis"] = "Changed"
    assert result["inputs"] == saved
    result["inputs"]["table"][1][1] = 88
    assert inputs["table"][1][1] == saved["table"][1][1]


@pytest.mark.parametrize("table", [[], [[0.002, 5], [0.001, 2]]])
def test_growth_validation_is_preserved(inputs, table):
    inputs["table"] = table
    with pytest.raises(ValueError):
        planning.plan_crack_inspection(**inputs)


def test_documented_synthetic_example():
    from html import unescape
    from pathlib import Path
    import re

    page = (
        Path(__file__).resolve().parents[2] / "docs/domains/asset-integrity/"
        "crack-inspection-policy.html"
    ).read_text(encoding="utf-8")
    code = re.search(r'<pre id="example">(.*?)</pre>', page, re.S).group(1)
    namespace = {}
    exec(compile(unescape(code), "documented example", "exec"), namespace)
    assert namespace["result"]["remaining_life_years"] == pytest.approx(1)
    assert namespace["result"]["next_inspection_interval_years"] == pytest.approx(0.5)
