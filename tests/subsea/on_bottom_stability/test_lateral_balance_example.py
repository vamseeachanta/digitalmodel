# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent arithmetic from lateral-balance-example.md, Hand calculation.

All physical inputs below are synthetic assumptions from that document's input
table. Invalid and zero inputs probe the documented applicability requirements;
they are not coefficients or limits taken from a standard.
"""

import json
import runpy
from pathlib import Path

import pytest


EXAMPLE = Path(__file__).resolve().parents[3] / "examples" / "lateral_balance.py"


@pytest.fixture
def assess():
    """Load the saved example without executing its command line entry point."""
    return runpy.run_path(str(EXAMPLE))["evaluate_balance"]


@pytest.fixture
def inputs():
    """Synthetic input table in the accompanying example documentation."""
    return {
        "submerged_weight_n_per_m": 1200.0,
        "horizontal_force_n_per_m": 400.0,
        "lift_force_n_per_m": 200.0,
        "friction_coefficient": 0.6,
        "safety_factor": 1.2,
    }


def test_lateral_balance_example(assess, inputs):
    """Recompute resistance and demand independently of the existing API."""
    # Hand calculation: R = 0.6 * (1200 - 200) = 600 N/m;
    # D = 1.2 * 400 = 480 N/m; U = 480 / 600 = 0.8.
    resistance = 0.6 * (1200.0 - 200.0)
    demand = 1.2 * 400.0
    result = assess(**inputs)
    assert result["resistance_n_per_m"] == pytest.approx(resistance)
    assert result["demand_n_per_m"] == pytest.approx(demand)
    assert result["utilization"] == pytest.approx(demand / resistance)
    assert result["utilization"] == pytest.approx(0.8)
    assert result["is_stable"] is True


def test_load_change_crosses_boundary(assess, inputs):
    """Only the horizontal load changes: 1.2 * 600 / 600 = 1.2."""
    accepted = assess(**inputs)
    inputs["horizontal_force_n_per_m"] = 600.0
    rejected = assess(**inputs)
    assert rejected["resistance_n_per_m"] == accepted["resistance_n_per_m"]
    assert rejected["demand_n_per_m"] == pytest.approx(1.2 * 600.0)
    assert rejected["utilization"] == pytest.approx(1.2)
    assert rejected["utilization"] > accepted["utilization"]
    assert rejected["is_stable"] is False


def test_exact_boundary_is_accepted(assess, inputs):
    """At H = 500 N/m, D = 1.2 * 500 = R = 600 N/m."""
    inputs["horizontal_force_n_per_m"] = 500.0
    result = assess(**inputs)
    assert result["demand_n_per_m"] == result["resistance_n_per_m"]
    assert result["utilization"] == pytest.approx(1.0)
    assert result["is_stable"] is True


def test_zero_load_limit(assess, inputs):
    """With H = L = 0, R = 0.6 * 1200 = 720 N/m and U = 0."""
    inputs.update(horizontal_force_n_per_m=0.0, lift_force_n_per_m=0.0)
    result = assess(**inputs)
    assert result["resistance_n_per_m"] == pytest.approx(0.6 * 1200.0)
    assert result["demand_n_per_m"] == 0.0
    assert result["utilization"] == 0.0
    assert result["is_stable"] is True


@pytest.mark.parametrize(
    ("field", "value"),
    [
        ("submerged_weight_n_per_m", 0.0),
        ("submerged_weight_n_per_m", -1200.0),
        ("horizontal_force_n_per_m", -400.0),
        ("lift_force_n_per_m", -200.0),
        ("lift_force_n_per_m", 1200.0),
        ("lift_force_n_per_m", 1400.0),  # W + L from the synthetic input table.
        ("friction_coefficient", 0.0),
        ("friction_coefficient", -0.6),
        ("safety_factor", 0.0),
        ("safety_factor", 0.8),  # Sub-unity probe, equal to the accepted U.
    ],
)
def test_zero_load_still_requires_applicable_inputs(assess, inputs, field, value):
    """Zero horizontal demand cannot waive the example's input requirements."""
    inputs.update(horizontal_force_n_per_m=0.0, lift_force_n_per_m=0.0)
    inputs[field] = value
    with pytest.raises(ValueError, match=field):
        assess(**inputs)


@pytest.mark.parametrize(
    "field",
    [
        "submerged_weight_n_per_m", "horizontal_force_n_per_m",
        "lift_force_n_per_m", "friction_coefficient", "safety_factor",
    ],
)
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf")])
def test_nonfinite_inputs_are_rejected(assess, inputs, field, value):
    """Nonfinite values cannot establish an applicable force balance."""
    inputs[field] = value
    with pytest.raises(ValueError, match=field):
        assess(**inputs)


@pytest.mark.parametrize("field", ["friction_coefficient", "safety_factor"])
def test_coefficients_are_required_even_at_zero_load(assess, inputs, field):
    """The example cannot silently adopt a friction or safety factor default."""
    inputs.update(horizontal_force_n_per_m=0.0, lift_force_n_per_m=0.0)
    del inputs[field]
    with pytest.raises(TypeError, match=field):
        assess(**inputs)


def test_saved_example_prints_both_sides_and_limits(capsys):
    """Every execution includes both verdicts and its applicability statement."""
    runpy.run_path(str(EXAMPLE), run_name="__main__")
    output = json.loads(capsys.readouterr().out)
    assert "synthetic assumptions" in output["input_basis"]
    assert "not DNV qualification" in output["applicability"]
    assert "positive friction" in output["applicability"]
    cases = output["cases"]
    assert [case["result"]["is_stable"] for case in cases] == [True, False, True]
    assert [case["result"]["utilization"] for case in cases] == pytest.approx(
        [0.8, 1.2, 0.0]
    )
    for case in cases:
        assert "friction_coefficient" in case["inputs"]
        assert "safety_factor" in case["inputs"]
