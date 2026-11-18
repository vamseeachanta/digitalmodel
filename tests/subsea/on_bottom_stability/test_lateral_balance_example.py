# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent arithmetic for OpenStax University Physics Volume 1 (2016).

Section 6.2, Eq. 6.1 and Example 6.10 supply the static friction basis.
Source forces are divided by one metre only to exercise the N/m API;
invalid and zero inputs probe the example's applicability requirements.
"""

import json
import runpy
from pathlib import Path

import pytest


EXAMPLE = Path(__file__).resolve().parents[3] / "examples" / "lateral_balance.py"


@pytest.fixture
def assess():
    """Load the saved example without executing its command line entry point."""
    return runpy.run_path(EXAMPLE.as_posix())["evaluate_balance"]


@pytest.fixture
def inputs():
    """Source case (c), normalized by one metre; no applied lift or factoring."""
    return {
        "submerged_weight_n_per_m": 196.0,
        "horizontal_force_n_per_m": 120.0,
        "lift_force_n_per_m": 0.0,
        "friction_coefficient": 0.700,
        "safety_factor": 1.0,
    }


def test_lateral_balance_example(assess, inputs):
    """Recompute resistance and demand independently of the existing API."""
    # Hand calculation: W = 20.0 * 9.80 = 196 N; R = 0.700 * 196 = 137.2 N;
    # D = 1 * 120 = 120 N; U = 120 / 137.2 = 300 / 343, about 0.874636.
    result = assess(**inputs)
    assert result["resistance_n_per_m"] == pytest.approx(137.2)
    assert result["demand_n_per_m"] == pytest.approx(120.0)
    assert result["utilization"] == pytest.approx(0.874636)
    assert result["is_stable"] is True


def test_load_change_crosses_boundary(assess, inputs):
    """Source case (d): H = 1.5 * 120 = 180; U = 180/137.2, about 1.311953."""
    accepted = assess(**inputs)
    inputs["horizontal_force_n_per_m"] = 180.0
    rejected = assess(**inputs)
    assert rejected["resistance_n_per_m"] == accepted["resistance_n_per_m"]
    assert rejected["demand_n_per_m"] == pytest.approx(180.0)
    assert rejected["utilization"] == pytest.approx(1.311953)
    assert rejected["utilization"] > accepted["utilization"]
    assert rejected["is_stable"] is False


def test_exact_boundary_is_accepted(assess, inputs):
    """At H = 137.2 N/m, unfactored demand equals the static capacity."""
    inputs["horizontal_force_n_per_m"] = 137.2
    result = assess(**inputs)
    assert result["demand_n_per_m"] == result["resistance_n_per_m"]
    assert result["utilization"] == pytest.approx(1.0)
    assert result["is_stable"] is True


def test_zero_load_limit(assess, inputs):
    """With H = L = 0, R = 0.700 * 196 = 137.2 N/m and U = 0."""
    inputs.update(horizontal_force_n_per_m=0.0, lift_force_n_per_m=0.0)
    result = assess(**inputs)
    assert result["resistance_n_per_m"] == pytest.approx(137.2)
    assert result["demand_n_per_m"] == 0.0
    assert result["utilization"] == 0.0
    assert result["is_stable"] is True


@pytest.mark.parametrize(
    ("field", "value"),
    [
        ("submerged_weight_n_per_m", 0.0),
        ("submerged_weight_n_per_m", -196.0),
        ("horizontal_force_n_per_m", -120.0),
        ("lift_force_n_per_m", -196.0),
        ("lift_force_n_per_m", 196.0),
        ("lift_force_n_per_m", 392.0),  # Twice the source weight.
        ("friction_coefficient", 0.0),
        ("friction_coefficient", -0.700),
        ("safety_factor", 0.0),
        ("safety_factor", 0.5),  # Half the unfactored source multiplier.
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
    runpy.run_path(EXAMPLE.as_posix(), run_name="__main__")
    output = json.loads(capsys.readouterr().out)
    assert "not DNV qualification" in output["applicability"]
    assert "positive friction" in output["applicability"]
    cases = output["cases"]
    assert [case["result"]["is_stable"] for case in cases] == [True, False, True]
    assert [case["result"]["utilization"] for case in cases] == pytest.approx(
        [0.875, 1.312, 0.0], abs=0.0005
    )
    assert [case["result"]["resistance_n_per_m"] for case in cases] == (
        pytest.approx([137.2, 137.2, 137.2])
    )
    assert [case["result"]["demand_n_per_m"] for case in cases] == (
        pytest.approx([120.0, 180.0, 0.0])
    )
    for case, load in zip(cases, [120.0, 180.0, 0.0], strict=True):
        assert case["inputs"] == {
            "submerged_weight_n_per_m": 196.0,
            "horizontal_force_n_per_m": load,
            "lift_force_n_per_m": 0.0,
            "friction_coefficient": 0.700,
            "safety_factor": 1.0,
        }


def test_saved_example_identifies_external_basis(capsys):
    """Saved results identify the worked example and the unit normalization."""
    runpy.run_path(EXAMPLE.as_posix(), run_name="__main__")
    output = json.loads(capsys.readouterr().out)
    assert "OpenStax" in output["input_basis"]
    assert "Example 6.10" in output["input_basis"]
    assert "one metre" in output["input_basis"]


def test_saved_example_states_criterion(capsys):
    """Verdicts carry the numerical static-capacity limit and its source."""
    runpy.run_path(EXAMPLE.as_posix(), run_name="__main__")
    output = json.loads(capsys.readouterr().out)
    assert "U <= 1.000" in output["criterion"]
    assert "OpenStax" in output["criterion"]
