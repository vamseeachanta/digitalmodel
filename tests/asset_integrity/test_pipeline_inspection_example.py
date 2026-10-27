# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent checks of the synthetic pipeline inspection arithmetic.

Synthetic input basis S1: previous wall 12 mm, current wall 11 mm,
elapsed time 4 yr, assumed required wall 8 mm, and supplied cap 3 yr.
The alternate cap 9 yr exercises the existing half-life convention.
Control S2 substitutes a wall of 7 mm, below the assumed required wall.
These are invented test inputs, not standard-derived or field measurements.
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


@pytest.fixture
def example():
    """Load the saved example without running its console entry point."""
    return runpy.run_path(str(EXAMPLE))


@pytest.mark.parametrize("cap", [3.0, 9.0])  # S1 and alternate cap above.
def test_pipeline_inspection_example_hand_life(example, cap):
    # Independent rational arithmetic from S1, never planner output.
    previous, current, elapsed, required = map(Fraction, (12, 11, 4, 8))
    rate = (previous - current) / elapsed  # 1/4 mm/yr.
    life = (current - required) / rate  # 12 yr.
    half_life = life / 2  # Existing planner convention, not a standard claim.

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
    assert plan["corrosion_rate_mm_yr"] == pytest.approx(float(rate))
    assert plan["remaining_life_years"] == pytest.approx(float(life))
    assert plan["half_life_interval_years"] == pytest.approx(float(half_life))
    assert plan["code_max_interval_years"] == cap
    assert plan["next_inspection_interval_years"] == min(float(half_life), cap)
    assert plan["screening_status"] == "pass"


def test_pipeline_inspection_example_below_required_wall(example):
    result = example["build_example"](max_interval_years=3.0)  # S1 cap.
    control = result["cases"]["below_required_wall"]
    inputs = control["inputs"]
    assert inputs["current_mm"]["value"] == 7.0  # S2.
    assert inputs["current_mm"]["value"] < inputs["required_mm"]["value"]
    # S2: (12 - 7) / 4 = 5/4 mm/yr; wall margin = 7 - 8 = -1 mm.
    plan = control["plan"]
    assert plan["corrosion_rate_mm_yr"] == pytest.approx(float(Fraction(5, 4)))
    assert plan["corrosion_allowance_remaining_mm"] == -1.0
    # Exhausted wall means no positive life or deferred inspection claim.
    assert plan["remaining_life_years"] == 0.0
    assert plan["half_life_interval_years"] == 0.0
    assert plan["next_inspection_interval_years"] == 0.0
    assert plan["screening_status"] == "fail"


def test_pipeline_inspection_example_explicit_input_basis(example):
    result = example["build_example"](max_interval_years=3.0)  # S1 cap.
    for case in result["cases"].values():
        inputs = case["inputs"]
        assert set(inputs) == {
            "previous_mm", "current_mm", "years_between", "required_mm",
            "max_interval_years",
        }
        for name, item in inputs.items():
            assert item["measured"] is False
            assert item["assumed"] is True
            assert "synthetic" in item["basis"].lower()
            assert item["unit"] == ("yr" if "years" in name else "mm")
        assert inputs["max_interval_years"]["value"] == 3.0


def test_pipeline_inspection_example_requires_supplied_cap(example):
    with pytest.raises(TypeError, match="max_interval_years"):
        example["build_example"]()


def test_pipeline_inspection_example_console_scope(capsys):
    runpy.run_path(str(EXAMPLE), run_name="__main__")
    result = json.loads(capsys.readouterr().out)
    assert "narrow integrity-management aid" in result["scope"]
    assert "does not establish DNV-RP-F116 compliance" in result["scope"]
    assert "qualified inspection interval" in result["scope"]
    assert "min(remaining_life / 2, supplied cap)" in result["planner_convention"]
    assert "not standard-derived" in result["planner_convention"]
    assert "repair" in result["cases"]["below_required_wall"]["way_forward"].lower()
