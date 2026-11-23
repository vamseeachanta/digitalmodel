# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent checks of a cited public thickness-planning worked example.

Numerical basis: the public article's "A worked example" section, linked below.
The exhausted-wall control reuses its required wall as the current wall.
"""

import json
import runpy
from fractions import Fraction
from pathlib import Path

import pytest


EXAMPLE = (
    Path(__file__).resolve().parents[2]
    / "examples/asset_integrity/example_pipeline_inspection.py"
)
SOURCE_URL = (
    "https://highpointsolutions.com.au/knowledge/"
    "how-often-should-ultrasonic-thickness-testing-be-done/"
)


@pytest.fixture
def example():
    """Load the saved example without running its console entry point."""
    return runpy.run_path(str(EXAMPLE))


@pytest.mark.parametrize("cap", [3.0, 3.8])  # Published schedule and comparison.
def test_pipeline_inspection_example_hand_life(example, cap):
    # Public worked example: (7.3 - 6.4) / 3 = 0.300 mm/yr;
    # (6.4 - 4.5) / 0.3 = 19/3 yr; half-life = 19/6 yr.
    previous, current, elapsed, required = map(Fraction, ("7.3", "6.4", "3", "4.5"))
    rate = (previous - current) / elapsed
    life = (current - required) / rate
    half_life = life / 2

    result = example["build_example"](max_interval_years=cap)
    inputs = result["cases"]["above_required_wall"]["inputs"]
    for name, expected in (
        ("previous_mm", previous),
        ("current_mm", current),
        ("years_between", elapsed),
        ("required_mm", required),
    ):
        assert inputs[name]["value"] == float(expected)
    plan = result["cases"]["above_required_wall"]["plan"]
    assert plan["corrosion_rate_mm_yr"] == pytest.approx(0.300)
    assert plan["corrosion_rate_mm_yr"] == pytest.approx(float(rate))
    assert plan["remaining_life_years"] == pytest.approx(6.333, abs=0.0005)
    assert plan["remaining_life_years"] == pytest.approx(float(life), abs=0.0005)
    assert plan["half_life_interval_years"] == pytest.approx(3.167, abs=0.0005)
    assert plan["half_life_interval_years"] == pytest.approx(float(half_life), abs=0.0005)
    assert plan["code_max_interval_years"] == cap
    assert plan["next_inspection_interval_years"] == pytest.approx(
        min(float(half_life), cap), abs=0.0005
    )
    assert plan["screening_status"] == "pass"


def test_pipeline_inspection_example_exhausted_wall(example):
    result = example["build_example"](max_interval_years=3.0)
    control = result["cases"]["at_required_wall"]
    inputs = control["inputs"]
    assert inputs["current_mm"]["value"] == 4.5
    assert inputs["current_mm"]["value"] == inputs["required_mm"]["value"]
    assert control["case_kind"] == "boundary_control"
    assert "not a reading" in inputs["current_mm"]["basis"]
    # Boundary: (7.3 - 4.5) / 3 = 14/15 mm/yr; margin = 4.5 - 4.5 = 0 mm.
    plan = control["plan"]
    assert plan["corrosion_rate_mm_yr"] == pytest.approx(float(Fraction(14, 15)), abs=0.0005)
    assert plan["corrosion_allowance_remaining_mm"] == 0.0
    # Exhausted wall means no positive life or deferred inspection claim.
    assert plan["remaining_life_years"] == 0.0
    assert plan["half_life_interval_years"] == 0.0
    assert plan["next_inspection_interval_years"] == 0.0
    assert plan["screening_status"] == "fail"


def test_pipeline_inspection_example_explicit_input_basis(example):
    result = example["build_example"](max_interval_years=3.0)
    assert result["source"]["url"] == SOURCE_URL
    assert result["source"]["published"] == "2026-09-09"
    assert result["source"]["section"] == "A worked example"
    for case in result["cases"].values():
        inputs = case["inputs"]
        assert set(inputs) == {
            "previous_mm", "current_mm", "years_between", "required_mm",
            "max_interval_years",
        }
        for name, item in inputs.items():
            assert item["measured"] is False
            assert item["assumed"] is True
            assert item["source_url"] == SOURCE_URL
            assert item["source_section"] == "A worked example"
            assert item["basis"]
            assert item["unit"] == ("yr" if "years" in name else "mm")
        assert inputs["max_interval_years"]["value"] == 3.0
        assert "suggested schedule" in inputs["max_interval_years"]["basis"]
    alternative = example["build_example"](max_interval_years=3.8)
    cap = alternative["cases"]["above_required_wall"]["inputs"]["max_interval_years"]
    assert "counterfactual" in cap["basis"]


def test_pipeline_inspection_example_requires_supplied_cap(example):
    with pytest.raises(TypeError, match="max_interval_years"):
        example["build_example"]()


@pytest.mark.parametrize(
    "cap", [-1.0, 0.0, 9.0, float("nan"), float("inf"), None, "3.0", True]
)
def test_pipeline_inspection_example_rejects_uncited_cap(example, cap):
    with pytest.raises(ValueError, match="cited"):
        example["build_example"](max_interval_years=cap)


def test_pipeline_inspection_example_console_scope(capsys):
    runpy.run_path(str(EXAMPLE), run_name="__main__")
    result = json.loads(capsys.readouterr().out)
    assert "narrow integrity-management aid" in result["scope"]
    assert "does not establish DNV-RP-F116 compliance" in result["scope"]
    assert "qualified inspection interval" in result["scope"]
    assert "min(remaining_life / 2, supplied cap)" in result["planner_convention"]
    assert "not standard-derived" in result["planner_convention"]
    assert result["source"]["url"] == SOURCE_URL
    assert "repair" in result["cases"]["at_required_wall"]["way_forward"].lower()
    for case in result["cases"].values():
        assert "current wall" in case["screening_basis"]
        assert "required wall 4.500 mm" in case["screening_basis"]
        assert "supplied cap 3.000 yr" in case["screening_basis"]
        for value in case["plan"].values():
            if isinstance(value, float):
                assert value == round(value, 3)
